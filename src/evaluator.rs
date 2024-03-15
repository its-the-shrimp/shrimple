use anyhow::{bail, ensure, Context};
use mlua::Value::Nil;
use mlua::{FromLua, Integer, Lua, Value};
use std::borrow::Cow;
use std::fmt::{self, Display, Formatter, Write};
use std::fs::ReadDir;
use std::mem::take;
use std::path::Path;

use crate::error::Locator;
use crate::parser::{Attr, AttrValue, ShrimpleParser};
use crate::utils::{cow_into, default, Prefixed, Result};

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

type XmlFragment<'src> = crate::parser::XmlFragment<'src, EvalCmd>;

pub trait Parser<'src>: Iterator<Item = XmlFragment<'src>> {}
impl<'src, T: Iterator<Item = XmlFragment<'src>>> Parser<'src> for T {}

pub trait StrLike<'src>: PartialEq<&'src str> + Display {}
impl<'src, T: PartialEq<&'src str> + Display> StrLike<'src> for T {}

// TODO: replace the traits above with the following when trait aliases are stabilised:
//
// trait Parser<'src> = Iterator<Item = XmlFragment<&'src str>>;
// trait StrLike<'src> = PartialEq<&'src str> + Display;

/// A "parser" (an iterator over tokens) + a queue for tokens to be fetched before progressing the
/// parser
struct TokenQueue<'locator, 'src> {
    parser: ShrimpleParser<'locator, 'src, EvalCmd>,
    queue: Vec<XmlFragment<'src>>,
    /// last fragment fetched from the queue
    last: Option<XmlFragment<'src>>,
}

impl<'locator, 'src> Iterator for TokenQueue<'locator, 'src> {
    type Item = XmlFragment<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.queue.pop().or_else(|| self.parser.next());
        if !matches!(res, Some(XmlFragment::Internal(_)) | None) {
            self.last = res;
        }
        res
    }
}

impl<'locator, 'src> TokenQueue<'locator, 'src> {
    fn process(
        src: &'src str,
        locator: &'locator Locator<'src>,
        f: impl FnOnce(&mut Self) -> Result,
    ) -> Result {
        let mut q = Self { parser: ShrimpleParser::new(src, locator), queue: vec![], last: None };
        let res = f(&mut q);
        q.parser.finish().and(res.map_err(|e| Self::wrap_error(e, locator, q.last)))
    }

    fn locator(&self) -> &'locator Locator<'src> {
        self.parser.locator()
    }

    fn enqueue<T>(&mut self, tokens: T)
    where
        T: IntoIterator<Item = XmlFragment<'src>>,
        T::IntoIter: DoubleEndedIterator,
    {
        self.queue.extend(tokens.into_iter().rev())
    }

    fn wrap_error(
        error: anyhow::Error,
        locator: &'locator Locator<'src>,
        at: Option<XmlFragment<'src>>,
    ) -> anyhow::Error {
        locator.wrap(error, at.map(|f| f.as_src_ptr()), "template expansion")
    }
}

#[derive(Clone)]
struct TemplateAttr<'src> {
    name: &'src str,
    default: Option<Cow<'src, str>>,
}

struct Template<'src> {
    name: &'src str,
    attrs: Box<[TemplateAttr<'src>]>,
    children: Box<[XmlFragment<'src>]>,
    accepts_children: bool,
}

struct Expansion<'src> {
    /// index in the evaluator's `templates`
    index: usize,
    children: Option<Box<[XmlFragment<'src>]>>,
}

struct IterCtx<'src> {
    var_name: &'src str,
    content: Box<[XmlFragment<'src>]>,
    iter: ReadDir,
}

#[derive(Default)]
pub struct Evaluator<'src> {
    /// files that are associated with the templates via `$ref` and which must be next to the
    /// templates themselves. Unlike `$xref`, these are NOT processed as template files
    refs: Vec<Cow<'src, Path>>,
    templates: Vec<Template<'src>>,
    /// A stack of currently expanded user-defined templates
    expansions: Vec<Expansion<'src>>,
    /// A stack of the contents of the currently expanded `$foreach` templates
    iterators: Vec<IterCtx<'src>>,
    lua_ctx: Lua,
}

/// For all `handle*template` functions:
/// the parser must be at the state of "<templateName....
///                                                  ^
impl<'src> Evaluator<'src> {
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

    fn eval_lua(&self, code: &str, locator: &Locator) -> Result<String> {
        Ok(match self.lua_ctx.load(code).set_name(locator.locate_ptr(code.as_ptr())).eval()? {
            Value::Nil => String::new(),
            res => String::from_lua(res, &self.lua_ctx)?,
        })
    }

    fn eval_attr_value<'attr>(
        &mut self,
        value: &AttrValue<'attr>,
        locator: &Locator,
    ) -> Result<Option<Cow<'attr, str>>> {
        Ok(match *value {
            AttrValue::None => None,
            AttrValue::Var(var) => Some(self.get_lua_var(var)?.into()),
            AttrValue::Expr(code) => Some(self.eval_lua(code, locator)?.into()),
            AttrValue::Text(text) => Some(text.into()),
        })
    }

    fn for_each_inner_xml(
        tag_name: impl StrLike<'src>,
        parser: impl Parser<'src>,
        mut f: impl FnMut(XmlFragment<'src>),
    ) -> Result {
        let mut nesting = 0usize;
        for frag in parser {
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

    fn collect_inner_xml(
        tag_name: impl StrLike<'src>,
        parser: &mut TokenQueue<'_, 'src>,
    ) -> Result<String> {
        let mut res = String::new();
        Self::for_each_inner_xml(tag_name, parser, |frag| _ = write!(&mut res, "{frag}"))?;
        Ok(res)
    }

    fn collect_inner_xml_fragments(
        tag_name: impl StrLike<'src>,
        parser: &mut TokenQueue<'_, 'src>,
    ) -> Result<Vec<XmlFragment<'src>>> {
        let mut res = vec![];
        Self::for_each_inner_xml(tag_name, parser, |frag| res.push(frag))?;
        Ok(res)
    }

    fn handle_ref_template(&mut self, parser: &mut TokenQueue<'_, 'src>) -> Result {
        let Attr { name, value } = match parser.next() {
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
            AttrValue::Expr(s) => self.eval_lua(s, parser.locator())?.into(),
            AttrValue::Text(s) => s.into(),
        };
        self.set_lua_var(name, &path)?;
        self.refs.push(cow_into(path));
        match parser.next() {
            Some(XmlFragment::OpeningTagEnd("/>")) => Ok(()),
            Some(XmlFragment::Attr(_)) => bail!("can't define more than 1 reference in a `$ref`"),
            Some(XmlFragment::OpeningTagEnd(">")) => bail!("`$ref` can't have children"),
            None => bail!("expected `/>`, instead got EOF"),
            _ => bail!("unexpected input"),
        }
    }

    fn handle_lua_template(
        &mut self,
        parser: &mut TokenQueue<'_, 'src>,
        dst: &mut impl Write,
    ) -> Result {
        match parser.next() {
            Some(XmlFragment::OpeningTagEnd(">")) => (),
            Some(XmlFragment::Attr(_)) => bail!("`$lua` doesn't accept any attributes"),
            Some(XmlFragment::OpeningTagEnd("/>")) => bail!("`$lua` must have children"),
            None => bail!("expected `>`, instead got EOF"),
            _ => bail!("unexpected input"),
        }
        dst.write_str(
            &self.eval_lua(&Self::collect_inner_xml("$lua", parser)?, parser.locator())?,
        )?;
        Ok(())
    }

    fn handle_template_template(&mut self, parser: &mut TokenQueue<'_, 'src>) -> Result {
        let mut accepts_children = false;
        let mut name: Option<&str> = None;
        let mut attrs = vec![];
        loop {
            match parser.next() {
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
                    attrs.push(TemplateAttr {
                        name,
                        default: self.eval_attr_value(&value, parser.locator())?,
                    })
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
            children: Self::collect_inner_xml_fragments("$template", parser)?.into(),
        });
        Ok(())
    }

    /// paste children passed to the invocation of the currently expanded template
    fn handle_children_template(&mut self, parser: &mut TokenQueue<'_, 'src>) -> Result {
        // TODO: can't use `$children` in the children provided to a template
        match parser.next() {
            Some(XmlFragment::OpeningTagEnd("/>")) => (),
            Some(XmlFragment::OpeningTagEnd(">")) => bail!("`$children` doesn't accept children"),
            Some(XmlFragment::Attr(_)) => bail!("`$children` doesn't accept any attributes"),
            None => bail!("expected `/>`, instead got EOF"),
            _ => bail!("unexpected input"),
        }
        let Some(Expansion { index, children }) = self.expansions.last() else {
            bail!("`$children` can only be used in the children of a template declaraion")
        };
        let Some(children) = children else {
            let name = self.templates[*index].name;
            bail!("`${name}` doesn't accept children so `$children` can't be used in it")
        };
        parser.enqueue(children.iter().copied());
        Ok(())
    }

    fn handle_foreach_template(&mut self, parser: &mut TokenQueue<'_, 'src>) -> Result {
        let var_name = match parser.next() {
            Some(XmlFragment::Attr(Attr { name, value: AttrValue::None })) => name
                .strip_prefix('$')
                .context("name of the declared variable must be prefixed with `$`")?,
            Some(XmlFragment::Attr(_)) => bail!("extraneous `=...`"),
            None => bail!("expected iterable variable name, instead got EOF"),
            _ => bail!("expected iterable variable name"),
        };
        ensure!(
            parser.next() == Some(XmlFragment::Attr(Attr { name: "in", value: AttrValue::None })),
            "expected `in`",
        );
        let dir: Cow<'src, str> = match parser.next() {
            Some(XmlFragment::Attr(Attr { name: "dir", value })) => self
                .eval_attr_value(&value, parser.locator())?
                .context("attribute `dir` must have a value that is the iterated directory")?,
            None => bail!("expected `dir=...`, instead got EOF"),
            _ => bail!("expected `dir=...`"),
        };
        let iter = parser
            .locator()
            .locate_path(&*dir)
            .read_dir()
            .with_context(|| format!("Failed to iterate directory {dir:?}"))?;
        ensure!(parser.next() == Some(XmlFragment::OpeningTagEnd(">")), "Expected `>`");

        self.iterators.push(IterCtx {
            var_name,
            content: Self::collect_inner_xml_fragments("$foreach", parser)?.into(),
            iter,
        });
        parser.enqueue([XmlFragment::Internal(EvalCmd::AdvanceIter)]);
        Ok(())
    }

    fn advance_iter(&mut self, parser: &mut TokenQueue<'_, 'src>) -> Result {
        let ctx = self
            .iterators
            .last_mut()
            .context("internal bug: no iteration context present for advancing")?;
        let var_name = ctx.var_name;
        let Some(next) = ctx.iter.next() else {
            self.remove_lua_var(var_name)?;
            self.remove_lua_var("index")?;
            self.iterators.pop();
            return Ok(());
        };
        let next = next.context("failed to advance the iterator")?.path();
        let next = next.to_str().with_context(|| format!("path {next:?} is not valid UTF-8"))?;

        parser.enqueue([XmlFragment::Internal(EvalCmd::AdvanceIter)]);
        parser.enqueue(ctx.content.iter().copied());
        self.set_lua_var(var_name, next)?;
        self.increment_lua_var("index")?;
        Ok(())
    }

    /// Handle a user-defined template
    fn handle_template(
        &mut self,
        // without the leading `$`
        name: &'src str,
        parser: &mut TokenQueue<'_, 'src>,
    ) -> Result {
        let (index, template) = self
            .templates
            .iter_mut()
            .enumerate()
            .find(|(_, t)| *t.name == *name)
            .with_context(|| format!("unknown template `${name}`"))?;
        let content = take(&mut template.children);
        let mut attrs = template.attrs.clone();
        let accepts_children = template.accepts_children;

        while let Some(frag) = parser.next() {
            match frag {
                XmlFragment::Attr(Attr { value: AttrValue::None, .. }) => {
                    bail!("user-defined templates' parameters must always have a value")
                }

                XmlFragment::Attr(attr) => {
                    let Some(id) = attrs.iter().position(|a| *a.name == *attr.name) else {
                        bail!("template `{name}` has no attribute `{}`", attr.name)
                    };
                    attrs[id].default = self.eval_attr_value(&attr.value, parser.locator())?;
                }

                XmlFragment::OpeningTagEnd(t) => {
                    for attr in &*attrs {
                        let Some(value) = &attr.default else {
                            bail!("`${name}`'s attribute `{}` received no value", attr.name)
                        };
                        self.set_lua_var(attr.name, value)?;
                    }
                    self.expansions.push(if t.starts_with('/') {
                        Expansion { index, children: accepts_children.then(default) }
                    } else {
                        ensure!(accepts_children, "`${name}` doesn't accept children");
                        Expansion {
                            index,
                            children: Self::collect_inner_xml_fragments(
                                Prefixed::<'$', _>(name),
                                parser,
                            )?
                            .into_boxed_slice()
                            .into(),
                        }
                    });
                    break;
                }

                _ => bail!("unexpected input"),
            }
        }

        parser.enqueue([XmlFragment::Internal(EvalCmd::EndExpansion)]);
        parser.enqueue(content.iter().copied());
        self.templates[index].children = content;
        Ok(())
    }

    fn end_expansion(&mut self) -> Result {
        self.expansions.pop().map(drop).context("internal bug: no template expansion to end")
    }

    fn eval_impl(&mut self, parser: &mut TokenQueue<'_, 'src>, dst: &mut impl Write) -> Result {
        let mut tag_stack: Vec<&str> = vec![];
        while let Some(frag) = parser.next() {
            match frag {
                XmlFragment::OpeningTagStart(name) => match name {
                    "$ref" => self.handle_ref_template(parser)?,
                    "$lua" => self.handle_lua_template(parser, dst)?,
                    "$template" => self.handle_template_template(parser)?,
                    "$children" => self.handle_children_template(parser)?,
                    "$foreach" => self.handle_foreach_template(parser)?,
                    _ => {
                        if let Some(name) = name.strip_prefix('$') {
                            self.handle_template(name, parser)?
                        } else {
                            tag_stack.push(name);
                            write!(dst, "<{name}")?
                        }
                    }
                },

                XmlFragment::Attr(Attr { name, value }) => {
                    dst.write_char(' ')?;
                    dst.write_str(name)?;
                    if let Some(value) = self.eval_attr_value(&value, parser.locator())? {
                        dst.write_str("=\"")?;
                        dst.write_str(&value)?;
                        dst.write_char('"')?;
                    }
                }

                XmlFragment::OpeningTagEnd(src) => {
                    if src.starts_with('/') {
                        tag_stack.pop();
                    }
                    dst.write_str(src)?
                }

                XmlFragment::ClosingTag(tag_name) => {
                    match tag_stack.pop() {
                        Some(name) if name == tag_name => (),
                        Some(name) => bail!("expected `</{name}>`, instead got `</{tag_name}>`"),
                        None => bail!("unmatched closing tag"),
                    }
                    write!(dst, "</{tag_name}>")?
                }

                XmlFragment::Var(name) => dst.write_str(&self.get_lua_var(name)?)?,

                XmlFragment::Expr(code) => {
                    dst.write_str(&self.eval_lua(code, parser.locator())?)?
                }

                XmlFragment::Text(text) => dst.write_str(text)?,

                XmlFragment::Internal(cmd) => match cmd {
                    EvalCmd::AdvanceIter => self.advance_iter(parser)?,
                    EvalCmd::EndExpansion => self.end_expansion()?,
                },
            }
        }

        Ok(())
    }

    pub fn eval(
        &mut self,
        input: &'src str,
        mut dst: impl Write,
        locator: &Locator<'src>,
    ) -> Result {
        TokenQueue::process(input, locator, |q| self.eval_impl(q, &mut dst))
    }
}
