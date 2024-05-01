use anyhow::{bail, ensure, Context};
use mlua::Value::Nil;
use mlua::{FromLua, Integer, Lua, Value};
use ureq::Agent;
use std::borrow::Cow;
use std::ffi::OsStr;
use std::fmt::{self, Display, Formatter, Write};
use std::fs::{create_dir, create_dir_all, read_dir, remove_file, write, ReadDir};
use std::io;
use std::mem::take;
use std::path::Path;
use std::ptr::null;
use DoubleEndedIterator as Reversible;

use crate::asset::{ptr_to_loc, Asset, AssetState};
use crate::mime::remote_file_ext;
use crate::parser::{url_scheme, Attr, AttrValue, ShrimpleParser};
use crate::short_str;
use crate::utils::{
    assume_static, assume_static_mut, default, os_str, soft_link, OptionExt, Prefixed, Result, ShortStr
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalCmd {
    AdvanceIter,
    EndExpansion,
}

impl Display for EvalCmd {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::AdvanceIter => write!(f, "$[__advanceIter]"),
            Self::EndExpansion => write!(f, "$[__endExpansion]"),
        }
    }
}

type XmlFragment = crate::parser::XmlFragment<'static, EvalCmd>;

pub trait StrLike<'src>: PartialEq<&'src str> + Display {}
impl<'src, T: PartialEq<&'src str> + Display> StrLike<'src> for T {}

// TODO: replace the traits above with the following when trait aliases are stabilised:
//
// trait Parser<'src> = Iterator<Item = XmlFragment<&'src str>>;
// trait StrLike<'src> = PartialEq<&'src str> + Display;

/// The parser + a queue for tokens to be fetched before progressing the parser
struct EvalCtx {
    parser: ShrimpleParser<EvalCmd>,
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
            self.last = res;
        }
        res
    }
}

impl EvalCtx {
    fn process<R>(
        src: &'static str,
        r: Asset,
        f: impl FnOnce(&mut Self) -> Result<R>,
    ) -> Result<R> {
        let mut ctx = Self {
            parser: ShrimpleParser::new(src, r),
            queue: vec![],
            expansions: vec![],
            iterators: vec![],
            last: None,
        };
        let res = f(&mut ctx);
        ctx.parser.finish().and(res.map_err(|e| Self::wrap_error(e, *ctx.file(), ctx.last)))
    }

    fn file(&self) -> &Asset {
        self.parser.file()
    }

    /// returns `src` not copied if it doesn't have anything that needs processing, otherwise
    /// returns a String built up by `f`
    fn format_str(
        &mut self,
        src: &'static str,
        f: impl FnOnce(&mut EvalCtx, &mut String) -> Result,
    ) -> Result<Cow<'static, str>> {
        EvalCtx::process(src, *self.file(), |ctx| {
            match ctx.parser.next() {
                None => return Ok(src.into()),
                Some(XmlFragment::Text(t)) if t.len() == src.len() => return Ok(src.into()),
                Some(frag) => ctx.enqueue([frag]),
            }
            let mut dst = String::new();
            f(ctx, &mut dst).map(|_| dst.into())
        })
    }

    /// Guarantees to not change `self.iterators` or `self.expansions`
    fn enqueue<T>(&mut self, tokens: T)
    where
        T: IntoIterator<Item = XmlFragment>,
        T::IntoIter: Reversible,
    {
        self.queue.extend(tokens.into_iter().rev())
    }
    // TODO: uncomment in 1.79.0
    //fn enqueue(&mut self, tokens: impl IntoIterator<Item = XmlFragment, IntoIter: Reversible>) {
    //    self.queue.extend(tokens.into_iter().rev())
    //}

    fn wrap_error(error: anyhow::Error, r: Asset, at: Option<XmlFragment>) -> anyhow::Error {
        r.wrap(error, at.map_or(null(), |f| f.as_src_ptr()), "template expansion")
    }

    fn end_expansion(&mut self) -> Result {
        self.expansions.pop().map(drop).context("internal bug: no template expansion to end")
    }
}

struct TemplateAttr {
    name: &'static str,
    default: Option<Cow<'static, str>>,
}

struct Template {
    name: &'static str,
    attrs: Box<[TemplateAttr]>,
    children: Box<[XmlFragment]>,
    accepts_children: bool,
}

struct Expansion {
    /// index in the evaluator's `templates`
    index: usize,
    children: Option<Box<[XmlFragment]>>,
}

struct IterCtx {
    var_name: &'static str,
    content: Box<[XmlFragment]>,
    iter: ReadDir,
}

pub struct Evaluator {
    templates: Vec<Template>,
    lua_ctx: Lua,
    processed_exts: Vec<&'static OsStr>,
    http_client: Agent,
    assets: Vec<Asset>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self {
            http_client: Agent::new(),
            templates: default(),
            lua_ctx: default(),
            processed_exts: vec![os_str("html"), os_str("css")],
            assets: vec![],
        }
    }
}

/// For all `handle*template` functions:
/// the parser must be at the state of "<templateName....
///                                                  ^
impl Evaluator {
    fn get_lua_var(&self, name: &str) -> Result<String> {
        Ok(self.lua_ctx.globals().get(name)?)
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

    fn eval_lua(&self, code: &str, file: Asset) -> Result<String> {
        Ok(match self.lua_ctx.load(code).set_name(file.locate(code.as_ptr())).eval()? {
            Value::Nil => String::new(),
            Value::Number(n) => {
                if n.is_finite() && n == n.trunc() {
                    (n as u64).to_string()
                } else {
                    n.to_string()
                }
            }
            res => String::from_lua(res, &self.lua_ctx)?,
        })
    }

    fn eval_attr_value(
        &mut self,
        value: &AttrValue<'static>,
        ctx: &mut EvalCtx,
    ) -> Result<Option<Cow<'static, str>>> {
        Ok(match *value {
            AttrValue::None => None,
            AttrValue::Var(var) => Some(self.get_lua_var(var)?.into()),
            AttrValue::Expr(code) => Some(self.eval_lua(code, *ctx.file())?.into()),
            AttrValue::Text(text) => {
                let res: Cow<'static, str> = ctx.format_str(
                    text,
                    |q, dst| self.eval(q, dst)
                )?;
                Some(res)
            }
        })
    }

    fn add_asset(&mut self, r: Asset) -> Result<usize> {
        Ok(if let Some(id) = self.assets.iter_mut().position(|r2| r2.path == r.path) {
            let existing = &mut self.assets[id];
            if existing.state == AssetState::Raw && r.state != AssetState::Raw {
                existing.state = r.state;
            }
            id
        } else {
            let id = self.assets.len();
            self.assets.push(r);
            id
        })
    }

    /// the return values are:
    /// (the index of the asset, the file extension of the asset)
    fn cache_remote_asset(&mut self, url: &str) -> Result<(usize, ShortStr)> {
        match url_scheme(url) {
            Some("http") | Some("https") => {
                let response = self.http_client.get(url).call()
                    .with_context(|| format!("failed to fetch {url:?}"))?;
                let ext = remote_file_ext(&response)
                    .with_context(|| format!("unable to infer the file type \
                                              of the URL {url:?}"))?
                    .to_owned();
                let mut content = vec![];
                response.into_reader().read_to_end(&mut content)?;
                let id = self.add_asset(
                    Asset::new_cached(url, content.leak(), ext)
                )?;
                Ok((id, ext))
            }
            Some(s) => {
                bail!("unable to cache, unknown URI scheme: {s:?}\n\
                       \tthe full URI is {url:?}")
            }
            None => {
                bail!("`$cached` can only be applied to remote assets\n\
                       \tthe full URI is {url:?}")
            }
        }
    }

    fn for_each_inner_xml<'tag_name>(
        tag_name: impl StrLike<'tag_name>,
        ctx: &mut EvalCtx,
        mut f: impl FnMut(XmlFragment),
    ) -> Result {
        let mut nesting = 0usize;
        for frag in ctx {
            match frag {
                XmlFragment::OpeningTagStart(t) if tag_name == t => nesting += 1,
                XmlFragment::ClosingTag(t) if tag_name == t => {
                    if nesting == 0 {
                        return Ok(());
                    } else {
                        nesting -= 1
                    }
                }
                _ => (),
            }
            f(frag);
        }
        bail!("expected `</{tag_name}>`, instead got EOF")
    }

    fn collect_inner_xml<'tag>(tag_name: impl StrLike<'tag>, ctx: &mut EvalCtx) -> Result<String> {
        let mut res = String::new();
        Self::for_each_inner_xml(tag_name, ctx, |frag| _ = write!(&mut res, "{frag}"))?;
        Ok(res)
    }

    fn collect_inner_xml_fragments<'tag>(
        tag_name: impl StrLike<'tag>,
        ctx: &mut EvalCtx,
    ) -> Result<Vec<XmlFragment>> {
        let mut res = vec![];
        Self::for_each_inner_xml(tag_name, ctx, |frag| res.push(frag))?;
        Ok(res)
    }

    fn collect_inner_xml_fragments_and_inspect<'tag>(
        tag_name: impl StrLike<'tag>,
        ctx: &mut EvalCtx,
        mut f: impl FnMut(&XmlFragment),
    ) -> Result<Vec<XmlFragment>> {
        let mut res = vec![];
        Self::for_each_inner_xml(tag_name, ctx, |frag| {
            f(&frag);
            res.push(frag);
        })?;
        Ok(res)
    }

    fn handle_ref_template(&mut self, ctx: &mut EvalCtx) -> Result {
        let mut attr = None;
        let mut remote_cached = false;
        let mut needs_processing = None;
        let Some(Attr { name, value }) = (loop {
            match ctx.next() {
                None => {
                    bail!("expected `<ref_name>=<ref_path>`, `raw`, `processed`, instead got EOF")
                }
                Some(XmlFragment::Attr(new_attr)) => match new_attr.name {
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
                    _ if attr.is_none() => {
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

        let Some(name) = name.strip_prefix('$') else {
            bail!("`$ref` only accepts a variable name prefixed with `$`, instead got {name:?}")
        };

        let path = self.eval_attr_value(&value, ctx)?.context("no file path provided")?;
        if remote_cached {
            let (id, ext) = self.cache_remote_asset(&path)?;
            let mut buf = short_str!("/cached/", len: 64);
            write!(&mut buf, "{id}{ext}")?;
            self.set_lua_var(name, buf.as_str())?;
        } else {
            self.set_lua_var(name, &path)?;
            self.add_asset(Asset::new(&*path, needs_processing, &self.processed_exts)?)?;
        }
        Ok(())
    }

    fn handle_lua_template(&mut self, ctx: &mut EvalCtx, dst: &mut impl Write) -> Result {
        match ctx.next() {
            Some(XmlFragment::OpeningTagEnd(">")) => (),
            Some(XmlFragment::Attr(_)) => bail!("`$lua` doesn't accept any attributes"),
            Some(XmlFragment::OpeningTagEnd("/>")) => bail!("`$lua` must have children"),
            None => bail!("expected `>`, instead got EOF"),
            _ => bail!("unexpected input"),
        }
        dst.write_str(&self.eval_lua(&Self::collect_inner_xml("$lua", ctx)?, *ctx.file())?)?;
        Ok(())
    }

    fn handle_template_template(&mut self, ctx: &mut EvalCtx) -> Result {
        let mut accepts_children = false;
        let mut name: Option<&str> = None;
        let mut attrs = vec![];
        let children = loop {
            match ctx.next() {
                None => bail!("expected attributes, `>` or `/>`, instead got EOF"),

                Some(XmlFragment::Attr(Attr { name: "acceptsChildren", value })) => {
                    ensure!(value == AttrValue::None, "`acceptsChildren` must have no value");
                    accepts_children = true
                }

                Some(XmlFragment::Attr(Attr { name: "name", value })) => match value {
                    AttrValue::None => bail!("no template name provided"),
                    AttrValue::Var(_) | AttrValue::Expr(_) => {
                        bail!("template name can only be a literal")
                    }
                    AttrValue::Text(text) => name = Some(text),
                },

                Some(XmlFragment::Attr(Attr { name, value })) => {
                    let Some(name) = name.strip_prefix('$') else {
                        bail!("`$template` doesn't have an attribute `{name}`")
                    };
                    attrs.push(TemplateAttr { name, default: self.eval_attr_value(&value, ctx)? })
                }

                Some(XmlFragment::OpeningTagEnd("/>")) => break default(),

                Some(XmlFragment::OpeningTagEnd(">")) => {
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
    fn handle_children_template(&mut self, ctx: &mut EvalCtx) -> Result {
        match ctx.next() {
            Some(XmlFragment::OpeningTagEnd("/>")) => (),
            Some(XmlFragment::OpeningTagEnd(">")) => bail!("`$children` doesn't accept children"),
            Some(XmlFragment::Attr(_)) => bail!("`$children` doesn't accept any attributes"),
            None => bail!("expected `/>`, instead got EOF"),
            _ => bail!("unexpected input"),
        }
        let Some(Expansion { index, children }) = ctx.expansions.last() else {
            bail!("`$children` can only be used in the children of a template declaraion")
        };
        let Some(children) = children else {
            let name = self.templates[*index].name;
            bail!("`${name}` doesn't accept children so `$children` can't be used in it")
        };
        // Safety: per `IterCtx::enqueue`, `ctx.expansions` won't be changed
        ctx.enqueue(unsafe {assume_static(children)}.iter().copied());
        Ok(())
    }

    fn handle_foreach_template(&mut self, ctx: &mut EvalCtx) -> Result {
        let var_name = match ctx.next() {
            Some(XmlFragment::Attr(Attr { name, value: AttrValue::None })) => name
                .strip_prefix('$')
                .context("name of the declared variable must be prefixed with `$`")?,
            Some(XmlFragment::Attr(_)) => bail!("extraneous `=...`"),
            None => bail!("expected iterable variable name, instead got EOF"),
            _ => bail!("expected iterable variable name"),
        };
        ensure!(
            ctx.next() == Some(XmlFragment::Attr(Attr { name: "in", value: AttrValue::None })),
            "expected `in`",
        );
        let dir = match ctx.next() {
            Some(XmlFragment::Attr(Attr { name: "dir", value })) => self
                .eval_attr_value(&value, ctx)?
                .context("attribute `dir` must have a value that is the iterated directory")?,
            None => bail!("expected `dir=...`, instead got EOF"),
            _ => bail!("expected `dir=...`"),
        };
        let iter =
            read_dir(&*dir).with_context(|| format!("Failed to iterate directory {dir:?}"))?;
        ensure!(ctx.next() == Some(XmlFragment::OpeningTagEnd(">")), "Expected `>`");

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
            Some(XmlFragment::Attr(Attr { name, value: AttrValue::None })) => name,
            Some(XmlFragment::Attr(_)) => {
                bail!("the argument must be the file extension without the dot, remove `=...`")
            }
            _ => bail!("expected file extension")
        };
        if ext.starts_with('.') {
            bail!("the file extension must not start with a dot")
        }
        ensure!(ctx.next() == Some(XmlFragment::OpeningTagEnd("/>")), "expected `/>`");

        self.processed_exts.push(os_str(ext));
        Ok(())
    }

    fn advance_iter(&mut self, ctx: &mut EvalCtx) -> Result {
        let iter = ctx
            .iterators
            .last_mut()
            .context("internal bug: no iteration context present for advancing")?;
        let var_name = iter.var_name;
        let Some(next) = iter.iter.next() else {
            self.remove_lua_var(var_name)?;
            self.remove_lua_var("index")?;
            ctx.iterators.pop();
            return Ok(());
        };
        let next = next.context("failed to advance the iterator")?.path();
        let next = next.to_str().with_context(|| format!("path {next:?} is not valid UTF-8"))?;

        // Safety: per `IterCtx::enqueue`, `ctx.iterators` won't be changed
        let iter = unsafe { assume_static_mut(iter) };
        ctx.enqueue([XmlFragment::Internal(EvalCmd::AdvanceIter)]);
        ctx.enqueue(iter.content.iter().copied());
        self.set_lua_var(var_name, next)?;
        self.increment_lua_var("index")?;
        Ok(())
    }

    /// Handle a user-defined template.
    /// `name` is without the leading `$`.
    fn handle_template(&mut self, name: &'static str, ctx: &mut EvalCtx) -> Result {
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
                    let Some(id) = attrs.iter().position(|a| *a.name == *attr.name) else {
                        bail!("template `{name}` has no attribute `{}`", attr.name)
                    };
                    attrs[id].default = self.eval_attr_value(&attr.value, ctx)?;
                }

                XmlFragment::OpeningTagEnd(t) => {
                    for attr in &*attrs {
                        let Some(value) = &attr.default else {
                            bail!("`${name}`'s attribute `{}` received no value", attr.name)
                        };
                        self.set_lua_var(attr.name, value)?;
                    }
                    let expansion = if t.starts_with('/') {
                        Expansion { index, children: accepts_children.then(default) }
                    } else {
                        ensure!(accepts_children, "`${name}` doesn't accept children");
                        let mut can_recurse = false;
                        let children = Self::collect_inner_xml_fragments_and_inspect(
                            Prefixed::<'$', _>(name),
                            ctx,
                            |f| can_recurse = match f {
                                XmlFragment::OpeningTagStart("$children") => true,
                                XmlFragment::OpeningTagEnd("$template") => false,
                                _ => return,
                            }
                        )?;
                        if can_recurse {
                            bail!("using `<$children>` in template children is disallowed \
                                   as it'd lead to infinite recursion")
                        }
                        Expansion {
                            index,
                            children: children.into_boxed_slice().into(),
                        }
                    };
                    ctx.expansions.push(expansion);
                    break;
                }

                _ => bail!("unexpected input"),
            }
        }

        ctx.enqueue([XmlFragment::Internal(EvalCmd::EndExpansion)]);
        ctx.enqueue(content.iter().copied());
        let template = &mut self.templates[index];
        template.children = content;
        template.attrs = attrs;
        Ok(())
    }

    /// `EMPTY` is defined per the HTML spec
    fn _handle_element<const IS_VOID: bool>(
        &mut self,
        name: &'static str,
        ctx: &mut EvalCtx,
        dst: &mut impl Write,
        // names of attributes that might contain a reference to a file
        ref_attrs: &[&str],
        tag_stack: &mut Vec<&'static str>,
    ) -> Result {
        write!(dst, "<{name}")?;
        let mut cached = false;
        while let Some(frag) = ctx.next() {
            match frag {
                XmlFragment::Attr(Attr { name: attr_name, value }) => {
                    let value = self.eval_attr_value(&value, ctx)?;
                    let Some(value) = value else {
                        match attr_name {
                            "$cached" => cached = true,
                            x => ensure!(!x.starts_with('$'), "unknown special attribute: {x:?}"),
                        }
                        if ref_attrs.contains(&attr_name) {
                            bail!("`{name}` element's `{attr_name}` attribute must have a value")
                        }
                        write!(dst, " {attr_name}")?;
                        continue;
                    };
                    write!(dst, " {attr_name}")?;

                    match (ref_attrs.contains(&attr_name), take(&mut cached)) {
                        (true, true) => match url_scheme(&value) { // ref attr, cached
                            Some("http") | Some("https") => {
                                let response = self.http_client.get(&value).call()
                                    .with_context(|| format!("failed to fetch {value:?}"))?;
                                let ext = remote_file_ext(&response)
                                    .with_context(|| format!("unable to infer the file type \
                                                              of the URL {value:?}"))?
                                    .to_owned();
                                let mut content = vec![];
                                response.into_reader().read_to_end(&mut content)?;
                                let id = self.add_asset(
                                    Asset::new_cached(value, content.leak(), ext)
                                )?;
                                write!(dst, "=\"/cached/{id}{ext}\"")?
                            }
                            Some(s) => {
                                bail!("unable to cache, unknown URI scheme: {s:?}\n\
                                       \tthe full URI is {value:?}")
                            }
                            None => {
                                bail!("`$cached` can only be applied to remote assets\n\
                                       \tthe full URI is {value:?}")
                            }
                        }

                        (true, false) => { // ref attr, not cached
                            self.add_asset(Asset::new(&*value, None, &self.processed_exts)?)?;
                            write!(dst, "=\"/{value}\"")?
                        }

                        (false, true) => { // not ref attr, cached
                            bail!("`$cached` can only be applied to a reference attribute\n\
                                   \tmore on reference attributes here: \
                                   https://github.com/schvv31n/shrimple/wiki/Assets")
                        }

                        (false, false) => { // not ref attr, not cached
                            if attr_name.starts_with('$') {
                                bail!("unknown special attribute: {attr_name:?}")
                            }
                            write!(dst, "=\"{value}\"")?
                        }
                    }
                }

                XmlFragment::OpeningTagEnd(t) => {
                    match (IS_VOID, t.starts_with('/')) {
                        (true, true) => dst.write_char('>')?,
                        (true, false) => bail!("element {name} must be self-closing"),
                        (false, true) => write!(dst, "></{name}>")?,
                        (false, false) => {
                            dst.write_char('>')?;
                            tag_stack.push(name);
                        }
                    }
                    return Ok(())
                }

                _ => bail!("expected attributes, `>` or `/>`"),
            }
        }
        bail!("expected attributes, `>` or `/>`, instead got EOF")
    }

    fn handle_element(
        &mut self,
        name: &'static str,
        ctx: &mut EvalCtx,
        dst: &mut impl Write,
        // names of attributes that might contain a reference to a file
        ref_attrs: &[&str],
        tag_stack: &mut Vec<&'static str>,
    ) -> Result {
        self._handle_element::<false>(name, ctx, dst, ref_attrs, tag_stack)
    }

    /// https://developer.mozilla.org/en-US/docs/Glossary/Void_element
    fn handle_void_element(
        &mut self,
        name: &'static str,
        ctx: &mut EvalCtx,
        dst: &mut impl Write,
        // names of attributes that might contain a reference to a file
        ref_attrs: &[&str],
    ) -> Result {
        self._handle_element::<true>(name, ctx, dst, ref_attrs, &mut vec![])
    }

    #[rustfmt::skip]
    fn eval(&mut self, ctx: &mut EvalCtx, dst: &mut impl Write) -> Result {
        let mut tag_stack: Vec<&str> = vec![];
        while let Some(frag) = ctx.next() {
            match frag {
                XmlFragment::OpeningTagStart(name) => match name {
                    "$ref" => self.handle_ref_template(ctx)?,
                    "$lua" => self.handle_lua_template(ctx, dst)?,
                    "$template" => self.handle_template_template(ctx)?,
                    "$children" => self.handle_children_template(ctx)?,
                    "$foreach" => self.handle_foreach_template(ctx)?,
                    "$registerProcessedExt" => self.handle_registerprocessedext_template(ctx)?,
                    "a"     |
                    "image" => self.handle_element(name, ctx, dst, &["href"], &mut tag_stack)?,
                    "link" => self.handle_void_element(name, ctx, dst, &["href"])?,
                    "img" => self.handle_void_element(name, ctx, dst, &["src"])?,
                    "!DOCTYPE" => bail!("<!DOCTYPE> is inserted automatically & \
                                         need not be specified explicitly"),
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
                    _ if name.starts_with('$') => self.handle_template(&name[1..], ctx)?,
                    _ => self.handle_element(name, ctx, dst, &[], &mut tag_stack)?,
                },

                XmlFragment::ClosingTag(tag_name) => {
                    match tag_stack.pop() {
                        Some(name) if name == tag_name => (),
                        Some(name) => bail!("expected `</{name}>`, instead got `</{tag_name}>`"),
                        None => bail!("unmatched closing tag"),
                    }
                    write!(dst, "</{tag_name}>")?
                }

                XmlFragment::Var(name) => dst.write_str(&self.get_lua_var(name)?)?,

                XmlFragment::Expr(code) => dst.write_str(&self.eval_lua(code, *ctx.file())?)?,

                XmlFragment::Text(text) => dst.write_str(text)?,

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
                let [line, col] = ptr_to_loc(src, tag.as_ptr().wrapping_sub(1));
                writeln!(err_msg, "\t{tag:?} at {line}:{col}")?;
            }
            bail!(err_msg)
        }
        Ok(())
    }

    fn eval_file(&mut self, ctx: &mut EvalCtx, dst: &mut impl Write) -> Result {
        if ctx.file().path.extension() == Some(os_str("html")) {
            dst.write_str("<!DOCTYPE html>\n")?;
        }
        self.eval(ctx, dst)
    }

    pub fn eval_all(
        mut self,
        src: impl AsRef<Path>,
        root: impl AsRef<Path>,
        dst: impl AsRef<Path>,
    ) -> Result {
        let src_root = root.as_ref();
        let mut dst_root = dst.as_ref().to_owned();
        self.add_asset(Asset::new_template(src)?)?;
        let mut dst = String::new();

        while let Some((src, file)) =
            self.assets.iter_mut().find_map(|x| x.src_for_processing().map(|s| (s, *x)))
        {
            dst.clear();
            EvalCtx::process(src, file, |ctx| self.eval_file(ctx, &mut dst))
                .with_context(|| format!("failed to evaluate templates in {:?}", file.path))?;
            let dst_path = dst_root.join(file.path.strip_prefix(src_root)?);
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
                            remove_file(&dst).with_context(|| format!("failed to remove {dst:?}"))?;
                            soft_link(src, &dst)?
                        } else {
                            bail!("failed to create a link from {src:?} to {dst:?}:\n\t{err}")
                        }
                    }
                }

                AssetState::Cached{content, ext} => {
                    dst_root.push("cached");
                    match create_dir(&dst_root) {
                        Err(err) if err.kind() != io::ErrorKind::AlreadyExists =>
                            bail!("failed to create a directory {dst_root:?}: {err}"),
                        _ => {},
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

                AssetState::Template(_) | AssetState::Processed(_) => {}
            }
        }

        Ok(())
    }
}
