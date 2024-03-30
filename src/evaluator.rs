use anyhow::{bail, ensure, Context};
use mlua::Value::Nil;
use mlua::{FromLua, Integer, Lua, Value};
use std::borrow::Cow;
use std::fmt::{self, Display, Formatter, Write};
use std::fs::{create_dir_all, hard_link, read_dir, remove_file, write, ReadDir};
use std::io;
use std::mem::take;
use std::path::Path;
use std::ptr::null;

use crate::file_ref::FileRepr;
use crate::parser::{url_scheme, Attr, AttrValue, ShrimpleParser};
use crate::utils::{
    assume_static, assume_static_mut, default, OptionExt, Prefixed, Result, VecExt,
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
struct EvalCtx<'files> {
    parser: ShrimpleParser<EvalCmd>,
    queue: Vec<XmlFragment>,
    /// Last fragment fetched from the queue
    last: Option<XmlFragment>,
    /// Already registered files
    files: &'files mut Vec<FileRepr>,
    /// A stack of currently expanded user-defined templates
    expansions: Vec<Expansion>,
    /// A stack of the contents of the currently expanded `$foreach` templates
    iterators: Vec<IterCtx>,
}

impl<'files> Iterator for EvalCtx<'files> {
    type Item = XmlFragment;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.queue.pop().or_else(|| self.parser.next());
        if !matches!(res, Some(XmlFragment::Internal(_)) | None) {
            self.last = res;
        }
        res
    }
}

impl<'files> EvalCtx<'files> {
    fn process<R>(
        src: &'static str,
        r: FileRepr,
        files: &'files mut Vec<FileRepr>,
        f: impl FnOnce(&mut Self) -> Result<R>,
    ) -> Result<R> {
        let mut ctx = Self {
            parser: ShrimpleParser::new(src, r),
            queue: vec![],
            expansions: vec![],
            iterators: vec![],
            last: None,
            files,
        };
        let res = f(&mut ctx);
        ctx.parser.finish().and(res.map_err(|e| Self::wrap_error(e, *ctx.file(), ctx.last)))
    }

    fn file(&self) -> &FileRepr {
        self.parser.file()
    }

    /// returns `src` not copied if it doesn't have anything that needs processing, otherwise
    /// returns a String built up by `f`
    fn format_str(
        &mut self,
        src: &'static str,
        f: impl FnOnce(&mut EvalCtx, &mut String) -> Result,
    ) -> Result<Cow<'static, str>> {
        EvalCtx::process(src, *self.file(), self.files, |ctx| {
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
        T::IntoIter: DoubleEndedIterator,
    {
        self.queue.extend(tokens.into_iter().rev())
    }

    fn wrap_error(error: anyhow::Error, r: FileRepr, at: Option<XmlFragment>) -> anyhow::Error {
        r.wrap(error, at.map_or(null(), |f| f.as_src_ptr()), "template expansion")
    }

    fn end_expansion(&mut self) -> Result {
        self.expansions.pop().map(drop).context("internal bug: no template expansion to end")
    }

    fn add_file(&mut self, r: FileRepr) -> Result<&FileRepr> {
        Ok(if let Some(id) = self.files.iter_mut().position(|r2| *r2.path == *r.path) {
            let existing = &mut self.files[id]; // Calms the crab, do not touch.
            if existing.state < r.state {
                existing.state = r.state;
            }
            existing
        } else {
            self.files.push_and_get(r)
        })
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

#[derive(Default)]
pub struct Evaluator {
    templates: Vec<Template>,
    // TODO: move to TokenQueue
    lua_ctx: Lua,
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

    fn eval_lua(&self, code: &str, file: FileRepr) -> Result<String> {
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
                let res: Cow<'static, str> = ctx.format_str(text, |q, dst| self.eval(q, dst))?;
                Some(res)
            }
        })
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
        bail!("expected `</{tag_name}`, instead got EOF")
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

    fn handle_ref_template(&mut self, ctx: &mut EvalCtx) -> Result {
        let Attr { name, value } = match ctx.next() {
            Some(XmlFragment::Attr(attr)) => attr,
            Some(XmlFragment::OpeningTagEnd(_)) => bail!("`$ref` expects exactly 1 attribute"),
            None => bail!("expected attributes or `/>`, instead got EOF"),
            _ => bail!("unexpected input"),
        };
        let Some(name) = name.strip_prefix('$') else {
            bail!("`$ref` only accepts a variable name prefixed with `$`, instead got {name:?}")
        };
        let path: Cow<'_, str> = match value {
            AttrValue::None => bail!("no file path provided"),
            AttrValue::Var(s) => self.get_lua_var(s)?.into(),
            AttrValue::Expr(s) => self.eval_lua(s, *ctx.file())?.into(),
            AttrValue::Text(s) => s.into(),
        };
        self.set_lua_var(name, &path)?;
        ctx.add_file(FileRepr::new(&*path, Some(false))?)?;
        match ctx.next() {
            Some(XmlFragment::OpeningTagEnd("/>")) => Ok(()),
            Some(XmlFragment::Attr(_)) => bail!("can't define more than 1 reference in a `$ref`"),
            Some(XmlFragment::OpeningTagEnd(">")) => bail!("`$ref` can't have children"),
            None => bail!("expected `/>`, instead got EOF"),
            _ => bail!("unexpected input"),
        }
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
        loop {
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

                Some(XmlFragment::OpeningTagEnd("/>")) => bail!("`$template` must have children"),

                Some(XmlFragment::OpeningTagEnd(">")) => break,

                _ => bail!("unexpected input"),
            }
        }

        self.templates.push(Template {
            name: name.context("no template name provided")?,
            accepts_children,
            attrs: attrs.into(),
            children: Self::collect_inner_xml_fragments("$template", ctx)?.into(),
        });
        Ok(())
    }

    /// paste children passed to the invocation of the currently expanded template
    fn handle_children_template(&mut self, ctx: &mut EvalCtx) -> Result {
        // TODO: can't use `$children` in the children provided to a template
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
        ctx.enqueue(unsafe { assume_static(children) }.iter().copied());
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
                        Expansion {
                            index,
                            children: Self::collect_inner_xml_fragments(
                                Prefixed::<'$', _>(name),
                                ctx,
                            )?
                            .into_boxed_slice()
                            .into(),
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
    fn _handle_element<const EMPTY: bool>(
        &mut self,
        name: &'static str,
        ctx: &mut EvalCtx,
        dst: &mut impl Write,
        // names of attributes that might contain a reference to a file
        ref_attrs: &[&str],
        tag_stack: &mut Vec<&'static str>,
    ) -> Result {
        write!(dst, "<{name}")?;
        while let Some(frag) = ctx.next() {
            match frag {
                XmlFragment::Attr(Attr { name: attr_name, value }) => {
                    write!(dst, " {attr_name}")?;
                    let value = self.eval_attr_value(&value, ctx)?;
                    if let Some(value) = value {
                        // TODO: implement remote asset caching
                        if ref_attrs.contains(&attr_name) && url_scheme(&value).is_none() {
                            ctx.add_file(FileRepr::new(&*value, None)?)?;
                            write!(dst, "=\"/{value}\"")?
                        } else {
                            write!(dst, "=\"{value}\"")?
                        }
                    } else if ref_attrs.contains(&attr_name) {
                        bail!("`{name}` element's `{attr_name}` attribute must have a value")
                    }
                }

                XmlFragment::OpeningTagEnd(t) => {
                    if t.starts_with('/') {
                        dst.write_str([" />", ">"][EMPTY as usize])?;
                        return Ok(());
                    } else if EMPTY {
                        bail!("element `{name}` must be self-closing")
                    } else {
                        tag_stack.push(name);
                        dst.write_char('>')?;
                        return Ok(());
                    }
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
                    "a"     |
                    "image" |
                    "link" => self.handle_element(name, ctx, dst, &["href"], &mut tag_stack)?,
                    // https://developer.mozilla.org/en-US/docs/Glossary/Void_element
                    "!DOCTYPE" |
                    "area"     |
                    "base"     |
                    "br"       |
                    "col"      |
                    "embed"    |
                    "hr"       |
                    "img"      |
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

        ensure!(tag_stack.is_empty(), "unclosed elements present: {tag_stack:?}");
        Ok(())
    }
}

pub fn eval(src: impl AsRef<Path>, root: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result {
    let src_root = root.as_ref();
    let dst_root = dst.as_ref();
    let mut files = vec![FileRepr::new(src, Some(true))?];
    let mut evaluator = Evaluator::default();
    let mut dst = String::new();

    while let Some((src, file)) =
        files.iter_mut().find_map(|x| x.src_for_processing().map(|s| (s, *x)))
    {
        dst.clear();
        EvalCtx::process(src, file, &mut files, |ctx| evaluator.eval(ctx, &mut dst))
            .with_context(|| format!("failed to evaluate templates in {:?}", file.path))?;
        let dst_path = dst_root.join(file.path.strip_prefix(src_root)?);
        dst_path.parent().try_map(create_dir_all)?;
        write(&dst_path, &dst).with_context(|| format!("failed to write to {dst_path:?}"))?;
    }

    for FileRepr { path: src, .. } in files.into_iter().filter(FileRepr::is_raw) {
        let dst = dst_root.join(src.strip_prefix(src_root)?);
        dst.parent().try_map(create_dir_all)?;
        match hard_link(src, &dst) {
            Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {
                remove_file(&dst).with_context(|| format!("failed to remove {dst:?}"))?;
                hard_link(src, &dst)
            }
            x => x,
        }
        .with_context(|| format!("failed to created a hard link from {src:?} to {dst:?}"))?;
    }

    Ok(())
}
