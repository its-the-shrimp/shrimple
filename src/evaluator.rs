use anyhow::{bail, ensure, Context, Error};
use mlua::Value::Nil;
use mlua::{FromLua, Integer, Lua, Value};
use shrimple_parser::tuple::Tuple;
use shrimple_parser::utils::{locate, locate_saturating, FullLocation, Location};
use shrimple_parser::Input;
use std::fmt::{Display, Write};
use std::fs::{create_dir, create_dir_all, read_dir, remove_file, write, ReadDir};
use std::io;
use std::mem::take;
use std::path::Path;
use std::ptr::null;
use ureq::Agent;

use crate::asset::{wrap_error, Asset, AssetState};
use crate::error::{collect_template_expansion_info, Expansions, ExtraCtx};
use crate::mime::remote_file_ext;
use crate::parser::{url_scheme, Attr, AttrValue, EvalCmd, ShrimpleParser, XmlFragment};
use crate::utils::{
    assume_static, assume_static_mut, default, os_str, soft_link, OptionExt, Prefixed, Result,
    ShortStr, StrExt,
};
use crate::view::{OsStrView, StrView, View};

pub trait StrLike: for<'src> PartialEq<&'src str> + Display + Copy {}
impl<T: for<'src> PartialEq<&'src str> + Display + Copy> StrLike for T {}

// TODO: replace the traits above with the following when trait aliases are stabilised:
//
// trait Parser<'src> = Iterator<Item = XmlFragment<&'src str>>;
// trait StrLike<'src> = PartialEq<&'src str> + Display;

/// The parser + a queue for tokens to be fetched before progressing the parser
struct EvalCtx {
    parser: ShrimpleParser,
    queue: Vec<XmlFragment>,
    /// Last fragment fetched from the queue
    last: Option<XmlFragment>,
    /// A stack of currently expanded user-defined templates
    expansions: Vec<Expansion>,
    /// A stack of the contents of the currently expanded `$foreach` templates
    iterators: Vec<IterCtx>,
}

impl Iterator for EvalCtx {
    type Item = XmlFragment;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.queue.pop().or_else(|| self.parser.next());
        if !matches!(res, Some(XmlFragment::Internal(_)) | None) {
            self.last.clone_from(&res);
        }
        res
    }
}

impl EvalCtx {
    fn process<R>(
        src: StrView,
        asset: Asset,
        f: impl FnOnce(&mut Self) -> Result<R>,
    ) -> Result<R> {
        let (path, asset_src) = (asset.path.clone(), asset.src());
        let mut ctx = Self {
            parser: ShrimpleParser::new(src, asset),
            queue: vec![],
            expansions: vec![],
            iterators: vec![],
            last: None,
        };
        let res = f(&mut ctx);
        let res = ctx.parser.finish().map_err(Error::new).and(res);
        let res = res.with_context(|| {
            Expansions(ctx.expansions.into_iter().map(|e| e.name).collect())
        });
        match (res, ctx.last, asset_src) {
            (Err(err), Some(f), Some(src)) => {
                if let Some(loc) = locate(f.as_src_ptr(), &src) {
                    let path = path.to_path_buf().into_os_string().into_encoded_bytes().into();
                    Err(err.context(ExtraCtx(FullLocation { path, loc })))
                } else {
                    Err(err)
                }
            }
            (res, ..) => res,
        }
    }

    const fn file(&self) -> &Asset {
        self.parser.file()
    }

    /// returns `src` not copied if it doesn't have anything that needs processing, otherwise
    /// returns a String built up by `f`
    fn format_str(&self, src: StrView, f: impl FnOnce(&mut Self, &mut String) -> Result)
        -> Result<StrView>
    {
        Self::process(src.clone(), self.file().clone(), |ctx| {
            match ctx.parser.next() {
                None => return Ok(src),
                Some(XmlFragment::Text(t)) if t.len() == src.len() => return Ok(src),
                Some(frag) => ctx.enqueue([frag]),
            }
            let mut dst = String::new();
            f(ctx, &mut dst).map(|_| dst.into())
        })
    }

    /// Guarantees to not change `self.iterators` or `self.expansions`
    fn enqueue(
        &mut self,
        tokens: impl IntoIterator<Item = XmlFragment, IntoIter: DoubleEndedIterator>,
    ) {
        self.queue.extend(tokens.into_iter().rev());
    }

    fn end_expansion(&mut self) -> Result {
        self.expansions.pop().map(drop).context("internal bug: no template expansion to end")
    }
}

struct TemplateAttr {
    name: StrView,
    default: Option<StrView>,
}

struct Template {
    name: StrView,
    attrs: Box<[TemplateAttr]>,
    children: Box<[XmlFragment]>,
    accepts_children: bool,
}

#[derive(Debug)]
struct Expansion {
    /// For error reporting purposes, the string's from the call site of the template.
    name: StrView,
    children: Option<Box<[XmlFragment]>>,
}

struct IterCtx {
    var_name: StrView,
    content: Box<[XmlFragment]>,
    iter: ReadDir,
}

struct Evaluator {
    templates: Vec<Template>,
    lua_ctx: Lua,
    processed_exts: Vec<OsStrView>,
    http_client: Agent,
    assets: Vec<Asset>,
}

impl Default for Evaluator {
    fn default() -> Self {
        let ext = |name| StrView::from(name).into_os_str_view();

        Self {
            http_client: Agent::new(),
            templates: default(),
            lua_ctx: default(),
            processed_exts: vec![ext("html"), ext("css"), ext("svg")],
            assets: vec![],
        }
    }
}

/// For all `handle*template` functions:
/// the parser must be at the state of "<templateName....
///                                                  ^
impl Evaluator {
    fn locate(&self, ptr: *const u8) -> Option<(View<'static, Path>, Location)> {
        shrimple_parser::utils::locate_in_multiple(
            ptr,
            self.assets.iter().map(|a| (a.path.clone(), a.src().unwrap_or_default())),
        )
    }

    fn get_lua_var(&self, name: &str) -> mlua::Result<String> {
        self.lua_ctx.globals().get(name)
    }

    fn set_lua_var(&self, name: &str, value: &str) -> mlua::Result<()> {
        self.lua_ctx.globals().set(name, value)
    }

    fn remove_lua_var(&self, name: &str) -> mlua::Result<()> {
        self.lua_ctx.globals().set(name, Nil)
    }

    /// if the variable is `nil`, it's set to 0
    fn increment_lua_var(&self, name: &str) -> Result {
        let globals = self.lua_ctx.globals();
        globals.set(
            name,
            match globals.get(name)? {
                Value::Nil => 0,
                val => Integer::from_lua(val, &self.lua_ctx)?
                    .checked_add(1)
                    .with_context(|| format!("integer overflow while incrementing `{name}`"))?,
            },
        )?;
        Ok(())
    }

    fn eval_lua(&self, code: &str) -> mlua::Result<String> {
        let name = if let Some((path, loc)) = self.locate(code.as_ptr()) {
            format!("{}:{loc}", path.display())
        } else {
            "<unknown>".to_owned()
        };

        match self.lua_ctx.load(code).set_name(name).eval()? {
            Value::Nil => Ok(default()),
            Value::Number(n) => Ok(n.to_string()),
            res => String::from_lua(res, &self.lua_ctx),
        }
    }

    fn eval_attr_value(
        &mut self,
        value: AttrValue,
        ctx: &EvalCtx,
    ) -> Result<Option<StrView>> {
        Ok(Some(match value {
            AttrValue::None => return Ok(None),
            AttrValue::Var(var) => self.get_lua_var(&var)?.into(),
            AttrValue::Expr(code) => self.eval_lua(&code)?.into(),
            AttrValue::Text(text) => ctx.format_str(text, |q, dst| {
                self.eval(q, dst).map_err(|e| {
                    let at = q.last.as_ref().map_or(null(), XmlFragment::as_src_ptr);
                    wrap_error(&self.assets, e, at, "string interpolation")
                })
            })?,
        }))
    }

    fn add_asset(&mut self, r: Asset) -> usize {
        if let Some(id) = self.assets.iter_mut().position(|r2| r2.path == r.path) {
            let existing = &mut self.assets[id];
            if existing.state == AssetState::Raw && r.state != AssetState::Raw {
                existing.state = r.state;
            }
            id
        } else {
            let id = self.assets.len();
            self.assets.push(r);
            id
        }
    }

    /// the return values are:
    /// (the index of the asset, the file extension of the asset)
    fn cache_remote_asset(&mut self, url: StrView) -> Result<(usize, ShortStr)> {
        match url_scheme(&url) {
            Some("http" | "https") => {
                let response = self
                    .http_client
                    .get(&url)
                    .call()
                    .with_context(|| format!("failed to fetch {url:?}"))?;
                let ext = remote_file_ext(&response).with_context(|| {
                    format!("unable to infer the file type of the URL {url:?}")
                })?;
                let mut content = vec![];
                response.into_reader().read_to_end(&mut content)?;
                let url = url.into_path_view();
                let id = self.add_asset(Asset::new_cached(url, content.into(), ext));
                Ok((id, ext))
            }
            Some(s) => {
                bail!(
                    "unable to cache, unknown URI scheme: {s:?}\n\
                       \tthe full URI is {url:?}"
                )
            }
            None => {
                bail!(
                    "`$cached` can only be applied to remote assets\n\
                       \tthe full URI is {url:?}"
                )
            }
        }
    }

    fn for_each_inner_xml(
        tag_name: impl StrLike,
        ctx: &mut EvalCtx,
        mut f: impl FnMut(XmlFragment) -> Result,
    ) -> Result {
        let mut nesting = 0usize;
        for frag in ctx.by_ref() {
            match &frag {
                XmlFragment::OpeningTagStart(t) if tag_name == t => nesting += 1,
                XmlFragment::ClosingTag(t) if tag_name == t => {
                    if nesting == 0 {
                        return Ok(());
                    }
                    nesting -= 1;
                }
                _ => (),
            }
            f(frag)?;
        }
        bail!("expected `</{tag_name}>`, instead got EOF")
    }

    fn collect_inner_xml(tag_name: impl StrLike, ctx: &mut EvalCtx) -> Result<String> {
        let mut res = String::new();
        Self::for_each_inner_xml(tag_name, ctx, |frag| Ok(write!(&mut res, "{frag}")?))?;
        Ok(res)
    }

    fn collect_inner_xml_fragments(
        tag_name: impl StrLike,
        ctx: &mut EvalCtx,
    ) -> Result<Vec<XmlFragment>> {
        let mut res = vec![];
        Self::for_each_inner_xml(tag_name, ctx, |frag| Ok(res.push(frag)))?;
        Ok(res)
    }

    fn collect_inner_xml_fragments_and_inspect(
        tag_name: impl StrLike,
        ctx: &mut EvalCtx,
        mut f: impl FnMut(&XmlFragment),
    ) -> Result<Vec<XmlFragment>> {
        let mut res = vec![];
        Self::for_each_inner_xml(tag_name, ctx, |frag| {
            Ok({
                f(&frag);
                res.push(frag);
            })
        })?;
        Ok(res)
    }

    fn handle_ref_template(&mut self, ctx: &mut EvalCtx) -> Result {
        let mut attr = None;
        let mut remote_cached = false;
        let mut needs_processing = None;
        let Some(attr) = (loop {
            match ctx.next() {
                None => {
                    bail!("expected `<ref_name>=<ref_path>`, `raw`, `processed`, instead got EOF")
                }
                Some(XmlFragment::Attr(new_attr)) => match new_attr.name() {
                    "raw" | "processed" if needs_processing.is_some() => {
                        bail!("can't specify more than 1 processing mode")
                    }
                    "raw" | "processed" if new_attr.has_value() => {
                        bail!("processing mode specifier must not have a value, remove `=...`")
                    }
                    "cached" if remote_cached => bail!("`cached` can only be provided once"),
                    "cached" => remote_cached = true,
                    "raw" => needs_processing = Some(false),
                    "processed" => needs_processing = Some(true),
                    _ if attr.is_some() => {
                        bail!("can't define more than 1 reference in a single `$ref` element")
                    }
                    _ => attr = Some(new_attr),
                },
                Some(XmlFragment::OpeningTagEnd(_)) => break attr,
                _ => bail!("invalid input, expected `<ref_name>=<ref_path>`, `raw`, `processed`"),
            }
        }) else {
            bail!("neither variable name nor path provided")
        };
        let (name, value) = attr.into_parts();

        let name = match name.strip_prefix('$') {
            Ok(x) => x,
            Err(x) => {
                bail!("`$ref` only accepts a variable name prefixed with `$`, instead got {x:?}")
            }
        };

        let path = self.eval_attr_value(value, ctx)?.context("no file path provided")?;
        if remote_cached {
            let (id, ext) = self.cache_remote_asset(path.clone())?;
            let mut buf = ShortStr::<64>::default();
            write!(&mut buf, "/cached/{id}.{ext}")?;
            self.set_lua_var(&name, buf.as_str())?;
        } else {
            self.set_lua_var(&name, &path)?;
            self.add_asset(Asset::new(&*path, needs_processing, &self.processed_exts)?);
        }
        Ok(())
    }

    fn handle_lua_template(&self, ctx: &mut EvalCtx, dst: &mut impl Write) -> Result {
        match ctx.next() {
            Some(XmlFragment::OpeningTagEnd(t)) if !t.is_self_closing() => (),
            Some(XmlFragment::Attr(_)) => bail!("`$lua` doesn't accept any attributes"),
            Some(XmlFragment::OpeningTagEnd(_)) => bail!("`$lua` must have children"),
            None => bail!("expected `>`, instead got EOF"),
            _ => bail!("unexpected input"),
        }
        dst.write_str(&self.eval_lua(&Self::collect_inner_xml("$lua", ctx)?)?)?;
        Ok(())
    }

    fn handle_template_template(&mut self, ctx: &mut EvalCtx) -> Result {
        let mut accepts_children = false;
        let mut name: Option<StrView> = None;
        let mut attrs = vec![];
        let children = loop {
            match ctx.next() {
                None => bail!("expected attributes, `>` or `/>`, instead got EOF"),

                Some(XmlFragment::Attr(attr)) => {
                    let (attr_name, value) = attr.into_parts();
                    match &*attr_name {
                        "acceptsChildren" => {
                            ensure!(value.is_none(), "`acceptsChildren` must have no value");
                            accepts_children = true;
                        }

                        "name" => match value {
                            AttrValue::None => bail!("no template name provided"),
                            AttrValue::Var(_) | AttrValue::Expr(_) => {
                                bail!("template name can only be a literal")
                            }
                            AttrValue::Text(text) => name = Some(text),
                        },

                        _ => {
                            let name = match attr_name.strip_prefix('$') {
                                Ok(x) => x,
                                Err(x) => bail!("`$template` doesn't have an attribute `{x}`"),
                            };
                            attrs.push(TemplateAttr {
                                name,
                                default: self.eval_attr_value(value, ctx)?,
                            });
                        }
                    }
                },

                Some(XmlFragment::OpeningTagEnd(t)) if t.is_self_closing() => break default(),

                Some(XmlFragment::OpeningTagEnd(_)) => {
                    break Self::collect_inner_xml_fragments("$template", ctx)?.into()
                }

                _ => bail!("unexpected input"),
            }
        };

        self.templates.push(Template {
            accepts_children,
            children,
            name: name.context("no template name provided")?,
            attrs: attrs.into(),
        });
        Ok(())
    }

    /// paste children passed to the invocation of the currently expanded template
    #[expect(clippy::unused_self, reason="consistency with other template handlers")]
    fn handle_children_template(&self, ctx: &mut EvalCtx, dst: &mut impl Write) -> Result {
        let raw = match ctx.next() {
            Some(XmlFragment::OpeningTagEnd(t)) if t.is_self_closing() => false,
            Some(XmlFragment::OpeningTagEnd(_)) => bail!("`$children` doesn't accept children"),
            Some(XmlFragment::Attr(a)) if a.name() == "raw" => {
                match ctx.next() {
                    Some(XmlFragment::OpeningTagEnd(t)) if t.is_self_closing() => (),
                    _ => bail!("expected `/>`"),
                }
                true
            }
            None => bail!("expected `raw` or `/>`, instead got EOF"),
            _ => bail!("unexpected input"),
        };

        let Some(Expansion { children, name, .. }) = ctx.expansions.last() else {
            bail!("`$children` can only be used in the children of a template declaraion")
        };
        let Some(children) = children else {
            bail!("`${name}` doesn't accept children so `$children` can't be used in it")
        };

        if raw {
            for frag in &**children {
                write!(dst, "{frag:#}")?;
            }
        } else {
            // Safety: per `IterCtx::enqueue`, `ctx.expansions` won't be changed
            ctx.enqueue(unsafe { assume_static(children) }.iter().cloned());
        }
        Ok(())
    }

    fn handle_foreach_template(&mut self, ctx: &mut EvalCtx) -> Result {
        let var_name = match ctx.next() {
            Some(XmlFragment::Attr(attr)) if !attr.has_value() => attr
                .into_parts().0
                .strip_prefix('$')
                .ok()
                .context("name of the declared variable must be prefixed with `$`")?,
            Some(XmlFragment::Attr(_)) => bail!("extraneous `=...`"),
            None => bail!("expected iterable variable name, instead got EOF"),
            _ => bail!("expected iterable variable name"),
        };
        match ctx.next() {
            Some(XmlFragment::Attr(a)) if !a.has_value() && a.name() == "in" => (),
            _ => bail!("expected `in`"),
        }
        let dir = match ctx.next() {
            Some(XmlFragment::Attr(a)) if a.name() == "dir" => self
                .eval_attr_value(a.value, ctx)?
                .context("attribute `dir` must have a value that is the iterated directory")?,
            None => bail!("expected `dir=...`, instead got EOF"),
            _ => bail!("expected `dir=...`"),
        };
        let iter =
            read_dir(&*dir).with_context(|| format!("Failed to iterate directory {dir:?}"))?;
        match ctx.next() {
            Some(XmlFragment::OpeningTagEnd(t)) if !t.is_self_closing() => (),
            _ => bail!("Expected `>`"),
        }

        let iter_ctx = IterCtx {
            var_name,
            content: Self::collect_inner_xml_fragments("$foreach", ctx)?.into(),
            iter,
        };
        ctx.iterators.push(iter_ctx);
        ctx.enqueue([XmlFragment::Internal(EvalCmd::AdvanceIter)]);
        Ok(())
    }

    fn handle_registerprocessedext_template(&mut self, ctx: &mut EvalCtx) -> Result {
        let ext = match ctx.next() {
            None => bail!("expected file extension, instead got EOF"),
            Some(XmlFragment::Attr(a)) if !a.has_value() => a.into_parts().0,
            Some(XmlFragment::Attr(_)) => {
                bail!("the argument must be the file extension without the dot, remove `=...`")
            }
            _ => bail!("expected file extension"),
        };
        if ext.starts_with('.') {
            bail!("the file extension must not start with a dot")
        }
        match ctx.next() {
            Some(XmlFragment::OpeningTagEnd(t)) if t.is_self_closing() => (),
            _ => bail!("expected `/>`"),
        }

        self.processed_exts.push(ext.into_os_str_view());
        Ok(())
    }

    fn advance_iter(&self, ctx: &mut EvalCtx) -> Result {
        let iter = ctx
            .iterators
            .last_mut()
            .context("internal bug: no iteration context present for advancing")?;
        let Some(next) = iter.iter.next() else {
            self.remove_lua_var(&iter.var_name)?;
            self.remove_lua_var("index")?;
            ctx.iterators.pop();
            return Ok(());
        };
        let next = next.context("failed to advance the iterator")?.path();
        let next = next.to_str().with_context(|| format!("path {next:?} is not valid UTF-8"))?;

        // Safety: per `IterCtx::enqueue`, `ctx.iterators` won't be changed
        let iter = unsafe { assume_static_mut(iter) };
        ctx.enqueue([XmlFragment::Internal(EvalCmd::AdvanceIter)]);
        ctx.enqueue(iter.content.iter().cloned());
        self.set_lua_var(&iter.var_name, next)?;
        self.increment_lua_var("index")?;
        Ok(())
    }

    #[expect(clippy::unused_self, reason="consistency with other template handlers")]
    fn handle_raw_template(&self, mut ctx: &mut EvalCtx, dst: &mut impl Write) -> Result {
        let element_name = match ctx.next() {
            Some(XmlFragment::Attr(attr)) if !attr.has_value() => {
                let name = attr.into_parts().0;
                write!(dst, "<{name}")?;
                for frag in &mut ctx {
                    write!(dst, "{frag}")?;
                    if matches!(frag, XmlFragment::OpeningTagEnd(_)) {
                        break;
                    }
                }
                Some(name)
            }
            Some(XmlFragment::Attr(_)) => {
                bail!("element name is an attribute without value\nremove the `=...`")
            }
            Some(XmlFragment::OpeningTagEnd(a)) if a.is_self_closing() => return Ok(()),
            Some(XmlFragment::OpeningTagEnd(_)) => {
                dst.write_char('>')?;
                None
            }
            Some(_) => bail!("expected `>`, `/>` or element name"),
            None => bail!("expected `>`, `/>` or element name, instead got EOF"),
        };
        Self::for_each_inner_xml("$raw", ctx, |frag| Ok(write!(dst, "{frag:#}")?))?;
        if let Some(element_name) = element_name {
            write!(dst, "</{element_name}>")?;
        }
        Ok(())
    }

    /// Handle a user-defined template.
    /// `name` is without the leading `$`.
    fn handle_template(&mut self, name: StrView, ctx: &mut EvalCtx) -> Result {
        let (index, template) = self
            .templates
            .iter_mut()
            .enumerate()
            .find(|(_, t)| *t.name == *name)
            .with_context(|| format!("unknown template `${name}`"))?;
        let content = take(&mut template.children);
        let mut attrs = take(&mut template.attrs);
        let accepts_children = template.accepts_children;

        while let Some(frag) = ctx.next() {
            match frag {
                XmlFragment::Attr(Attr { value: AttrValue::None, .. }) => {
                    bail!("user-defined templates' parameters must always have a value")
                }

                XmlFragment::Attr(attr) => {
                    let attr_name = attr.name();
                    let Some(id) = attrs.iter().position(|a| *a.name == *attr_name) else {
                        bail!("template `{name}` has no attribute `{attr_name}`")
                    };
                    attrs[id].default = self.eval_attr_value(attr.value, ctx)?;
                }

                XmlFragment::OpeningTagEnd(t) => {
                    for attr in &*attrs {
                        let Some(value) = &attr.default else {
                            bail!("`${name}`'s attribute `{}` received no value", attr.name)
                        };
                        self.set_lua_var(&attr.name, value)?;
                    }
                    let expansion = if t.is_self_closing() {
                        Expansion { name, children: accepts_children.then(default) }
                    } else {
                        ensure!(accepts_children, "`${name}` doesn't accept children");
                        let mut can_recurse = false;
                        let children = Self::collect_inner_xml_fragments_and_inspect(
                            Prefixed::<'$', _>(&*name),
                            ctx,
                            |f| {
                                can_recurse = match f {
                                    XmlFragment::OpeningTagStart(name) if *name == "$children" => true,
                                    XmlFragment::ClosingTag(name) if *name == "$template" => false,
                                    _ => return,
                                }
                            },
                        )?;
                        if can_recurse {
                            // TODO: report the location of this error
                            bail!(
                                "using `<$children>` in template children is disallowed \
                                   as it'd lead to infinite recursion"
                            )
                        }

                        Expansion { name, children: children.into_boxed_slice().into() }
                    };
                    ctx.expansions.push(expansion);
                    break;
                }

                _ => bail!("unexpected input"),
            }
        }

        ctx.enqueue([XmlFragment::Internal(EvalCmd::EndExpansion)]);
        ctx.enqueue(content.iter().cloned());
        let template = &mut self.templates[index];
        template.children = content;
        template.attrs = attrs;
        Ok(())
    }

    /// `IS_VOID` is defined per the HTML spec
    fn _handle_element<const IS_VOID: bool>(
        &mut self,
        name: StrView,
        ctx: &mut EvalCtx,
        dst: &mut impl Write,
        // names of attributes that might contain a reference to a file
        ref_attrs: &[&str],
        tag_stack: &mut Vec<StrView>,
    ) -> Result {
        write!(dst, "<{name}")?;
        let mut cached = false;
        while let Some(frag) = ctx.next() {
            match frag {
                XmlFragment::Attr(attr) => {
                    let (attr_name, value) = attr.into_parts();
                    let value = self.eval_attr_value(value, ctx)?;
                    let Some(value) = value else {
                        match &*attr_name {
                            "$cached" => cached = true,
                            x => ensure!(!x.starts_with('$'), "unknown special attribute: {x:?}"),
                        }
                        if ref_attrs.contains(&&*attr_name) {
                            bail!("`{name}` element's `{attr_name}` attribute must have a value")
                        }
                        write!(dst, " {attr_name}")?;
                        continue;
                    };
                    write!(dst, " {attr_name}")?;

                    match (ref_attrs.contains(&&*attr_name), take(&mut cached)) {
                        (true, true) => { // ref attr, cached
                            let (id, ext) = self.cache_remote_asset(value)?;
                            write!(dst, "=\"/cached/{id}.{ext}\"")?;
                        }

                        (true, false) => { // ref attr, not cached
                            let trimmed = value.trim_start_matches('/').trim_fragment();
                            if !trimmed.is_empty() && url_scheme(trimmed).is_none() {
                                self.add_asset(Asset::new(trimmed, None, &self.processed_exts)?);
                            }
                            write!(dst, "=\"{value}\"")?;
                        }

                        (false, true) => { // not ref attr, cached
                            bail!(
                                "`$cached` can only be applied to a reference attribute\n\
                                   \tmore on reference attributes here: \
                                   https://github.com/schvv31n/shrimple/wiki/Assets"
                            )
                        }

                        (false, false) => { // not ref attr, not cached
                            if attr_name.starts_with('$') {
                                bail!("unknown special attribute: {attr_name:?}")
                            }
                            write!(dst, "=\"{value}\"")?;
                        }
                    }
                }

                XmlFragment::OpeningTagEnd(t) => {
                    match (IS_VOID, t.is_self_closing()) {
                        (true, true) => dst.write_char('>')?,
                        (true, false) => bail!("element `<{name}>` must be self-closing"),
                        (false, true) => write!(dst, "></{name}>")?,
                        (false, false) => {
                            dst.write_char('>')?;
                            tag_stack.push(name);
                        }
                    }
                    return Ok(());
                }

                _ => bail!("expected attributes, `>` or `/>`"),
            }
        }
        bail!("expected attributes, `>` or `/>`, instead got EOF")
    }

    fn handle_element(
        &mut self,
        name: StrView,
        ctx: &mut EvalCtx,
        dst: &mut impl Write,
        // names of attributes that might contain a reference to a file
        ref_attrs: &[&str],
        tag_stack: &mut Vec<StrView>,
    ) -> Result {
        self._handle_element::<false>(name, ctx, dst, ref_attrs, tag_stack)
    }

    /// <https://developer.mozilla.org/en-US/docs/Glossary/Void_element>
    fn handle_void_element(
        &mut self,
        name: StrView,
        ctx: &mut EvalCtx,
        dst: &mut impl Write,
        // names of attributes that might contain a reference to a file
        ref_attrs: &[&str],
    ) -> Result {
        self._handle_element::<true>(name, ctx, dst, ref_attrs, &mut vec![])
    }

    #[rustfmt::skip]
    fn eval(&mut self, ctx: &mut EvalCtx, dst: &mut impl Write) -> Result {
        let mut tag_stack = vec![];
        while let Some(frag) = ctx.next() {
            match frag {
                XmlFragment::OpeningTagStart(name) => match &*name {
                    "$ref" => self.handle_ref_template(ctx)?,
                    "$lua" => self.handle_lua_template(ctx, dst)?,
                    "$template" => self.handle_template_template(ctx)?,
                    "$children" => self.handle_children_template(ctx, dst)?,
                    "$foreach" => self.handle_foreach_template(ctx)?,
                    "$registerProcessedExt" => self.handle_registerprocessedext_template(ctx)?,
                    "$raw" => self.handle_raw_template(ctx, dst)?,

                    "a"     |
                    "image" |
                    "use" => self.handle_element(name, ctx, dst, &["href"], &mut tag_stack)?,
                    "link" => self.handle_void_element(name, ctx, dst, &["href"])?,
                    "img" => self.handle_void_element(name, ctx, dst, &["src"])?,
                    "script" => self.handle_element(name, ctx, dst, &["src"], &mut tag_stack)?,
                    "form" => self.handle_element(name, ctx, dst, &["action"], &mut tag_stack)?,

                    "!DOCTYPE" |
                    "html" => ensure!(
                        ctx.file().path.extension() != Some(os_str("html")),
                        "<{name}> is inserted automatically & need not be specified explicitly"
                    ),

                    "area"     |
                    "base"     |
                    "br"       |
                    "col"      |
                    "embed"    |
                    "hr"       |
                    "input"    |
                    "meta"     |
                    "param"    |
                    "source"   |
                    "track"    |
                    "wbr"      => self.handle_void_element(name, ctx, dst, &[])?,

                    "" => bail!("element name cannot be empty"),

                    _ if name.starts_with('$') => self.handle_template(name.after(1), ctx)?,

                    _ => self.handle_element(name, ctx, dst, &[], &mut tag_stack)?,
                },

                XmlFragment::ClosingTag(tag_name) => {
                    match tag_stack.pop() {
                        Some(name) if name == tag_name => (),
                        Some(name) => bail!("expected `</{name}>`, instead got `</{tag_name}>`"),
                        None => bail!("unmatched closing tag"),
                    }
                    write!(dst, "</{tag_name}>")?;
                }

                XmlFragment::Var(name) => dst.write_str(&self.get_lua_var(&name)?)?,

                XmlFragment::Expr(code) => dst.write_str(&self.eval_lua(&code)?)?,

                XmlFragment::Text(text) => dst.write_str(&text)?,

                XmlFragment::Internal(cmd) => match cmd {
                    EvalCmd::AdvanceIter => self.advance_iter(ctx)?,
                    EvalCmd::EndExpansion => ctx.end_expansion()?,
                },

                XmlFragment::Attr(_) | XmlFragment::OpeningTagEnd(_) => bail!("unexpected input"),
            }
        }

        if !tag_stack.is_empty() {
            let mut err_msg = "unclosed elements present:\n".to_owned();
            let src = ctx.parser.file().src().context("internal bug: no source code found")?;
            for tag in tag_stack {
                writeln!(err_msg, "\t{tag:?} at {}", locate_saturating(tag.as_ptr(), &src))?;
            }
            bail!(err_msg)
        }
        Ok(())
    }

    fn eval_file(&mut self, ctx: &mut EvalCtx, dst: &mut impl Write) -> Result {
        let is_html = ctx.file().path.extension() == Some(os_str("html"));
        /*if ctx.file().path.extension() == Some(os_str("css")) {
            for x in ctx.parser.clone() {
                println!("{x:?}");
            }
        }*/
        if is_html {
            dst.write_str("\
                <!DOCTYPE html>\n\
                <html>\n\
                \t<head>\n\
                \t\t<meta charset=\"UTF-8\">\n\
                \t</head>\n\
                ")?;
        }
        self.eval(ctx, dst)?;
        if is_html {
            dst.write_str("\n</html>\n")?;
        }
        Ok(())
    }

    pub fn eval_all(
        &mut self,
        src: impl AsRef<Path>,
        root: impl AsRef<Path>,
        dst: impl AsRef<Path>,
    ) -> Result {
        let src_root = root.as_ref();
        let mut dst_root = dst.as_ref().to_owned();
        self.add_asset(Asset::new_template(src)?);
        let mut dst = String::new();

        while let Some((src, file)) =
            self.assets.iter_mut().find_map(|x| x.src_for_processing().map(|s| (s, x.clone())))
        {
            dst.clear();
            let dst_path = dst_root.join(file.path.strip_prefix(src_root)?);
            EvalCtx::process(src, file, |ctx| self.eval_file(ctx, &mut dst))?;
            dst_path.parent().try_map(create_dir_all)?;
            write(&dst_path, &dst).with_context(|| format!("failed to write to {dst_path:?}"))?;
        }

        for (id, Asset { path: src, state }) in self.assets.iter().enumerate() {
            match state {
                AssetState::Raw => {
                    let dst = dst_root.join(src.strip_prefix(src_root)?);
                    dst.parent().try_map(create_dir_all)?;
                    if let Err(err) = soft_link(src, &dst) {
                        if err.kind() == io::ErrorKind::AlreadyExists {
                            remove_file(&dst)
                                .with_context(|| format!("failed to remove {dst:?}"))?;
                            soft_link(src, &dst)?;
                        } else {
                            bail!("failed to create a link from {src:?} to {dst:?}:\n\t{err}")
                        }
                    }
                }

                AssetState::Cached { content, ext } => {
                    dst_root.push("cached");
                    match create_dir(&dst_root) {
                        Err(err) if err.kind() != io::ErrorKind::AlreadyExists => {
                            bail!("failed to create a directory {dst_root:?}: {err}")
                        }
                        _ => {}
                    }
                    let mut num = ShortStr::<32>::default();
                    write!(&mut num, "{id}")?;
                    dst_root.push(num.as_str());
                    if let Some(ext) = ext.as_str().get(1..) {
                        dst_root.set_extension(ext);
                    }
                    write(&dst_root, content)?;
                    dst_root.pop();
                    dst_root.pop();
                }

                AssetState::Template(_) | AssetState::Processed => {}
            }
        }

        Ok(())
    }
}

pub fn eval(src: impl AsRef<Path>, root: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result {
    let mut evaluator = Evaluator::default();
    evaluator.eval_all(src, root, dst).map_err(|e| {
        collect_template_expansion_info(
            &e,
            evaluator.assets.into_iter().map(|a| (a.src().unwrap_or_default(), a.path).rev()),
        )
    })
}
