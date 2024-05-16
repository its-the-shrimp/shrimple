use shrimple_parser::{
    from_tuple, parse_char, parse_exact, parse_group, parse_until, parse_until_ex,
    parse_until_exact, parse_while,
    tuple::{first, second},
    utils::{eq, ne},
    FullParsingError, Parser,
};

use crate::{
    asset::Asset,
    escape_html::EscapeHtml,
    utils::{is_in, Result},
};
use std::{
    convert::Infallible,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    ptr::null,
};

#[derive(Debug, PartialEq, Eq, Default, Clone, Copy)]
pub enum AttrValue<'src> {
    /// No value
    #[default]
    None,
    /// Without the `$`
    Var(&'src str),
    /// Without the `$()`
    Expr(&'src str),
    /// String literal or a word terminated by whitespace, `/`, `>`
    Text(&'src str),
}

impl Display for AttrValue<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            match self {
                Self::None => Ok(()),
                Self::Var(name) => write!(f, "${}", EscapeHtml(name)),
                Self::Expr(expr) => write!(f, "$({})", EscapeHtml(expr)),
                Self::Text(text) => Debug::fmt(&EscapeHtml(text), f),
            }
        } else {
            match self {
                Self::None => Ok(()),
                Self::Var(name) => write!(f, "${name}"),
                Self::Expr(expr) => write!(f, "$({expr})"),
                Self::Text(text) => Debug::fmt(text, f),
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Attr<'src> {
    prefix_and_name: &'src str,
    /// Safety: `self.name_start < self.prefix_and_name.len()`
    name_start: usize,
    pub value: AttrValue<'src>,
}

impl Display for Attr<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            if self.has_value() {
                write!(f, "{}={:#}", EscapeHtml(self.prefix_and_name), self.value)
            } else {
                Display::fmt(&EscapeHtml(self.prefix_and_name), f)
            }
        } else if self.has_value() {
            write!(f, "{}={}", self.prefix_and_name, self.value)
        } else {
            Display::fmt(self.prefix_and_name, f)
        }
    }
}

impl<'src> Attr<'src> {
    pub const fn has_value(&self) -> bool {
        !matches!(self.value, AttrValue::None)
    }

    pub const fn as_src_ptr(&self) -> *const u8 {
        self.prefix_and_name.as_ptr()
    }

    pub fn name(&self) -> &'src str {
        &self.prefix_and_name[self.name_start..]
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
// Safety invariant: `!self.0.is_empty()`
pub struct OpeningTagEnd<'src>(&'src str);

impl Display for OpeningTagEnd<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            Display::fmt(&EscapeHtml(self.0), f)
        } else {
            Display::fmt(self.0, f)
        }
    }
}

impl OpeningTagEnd<'_> {
    pub fn as_src_ptr(&self) -> *const u8 {
        self.0.as_ptr().wrapping_add(self.0.len() - 1 - self.0.starts_with("/>") as usize)
    }

    pub fn is_self_closing(&self) -> bool {
        self.0.ends_with("/>")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvalCmd {
    AdvanceIter,
    EndExpansion,
}

impl Display for EvalCmd {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::AdvanceIter => write!(f, "$[__advanceIter]"),
            Self::EndExpansion => write!(f, "$[__endExpansion]"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum XmlFragment<'src> {
    /// <tagname
    OpeningTagStart(&'src str),
    /// `key=value` or key
    Attr(Attr<'src>),
    /// `/>` or `>`
    OpeningTagEnd(OpeningTagEnd<'src>),
    /// </tagname>
    ClosingTag(&'src str),
    /// $VARNAME
    Var(&'src str),
    /// $(LUA CODE)
    Expr(&'src str),
    /// any other text
    Text(&'src str),
    /// used by the evaluator
    Internal(EvalCmd),
}

impl Display for XmlFragment<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            match self {
                Self::OpeningTagStart(name) => write!(f, "&lt;{}", EscapeHtml(name)),
                Self::Attr(attr) => write!(f, "{attr:#}"),
                Self::OpeningTagEnd(s) => write!(f, "{s:#}"),
                Self::ClosingTag(name) => write!(f, "&lt;/{}&gt;", EscapeHtml(name)),
                Self::Var(name) => write!(f, "${}", EscapeHtml(name)),
                Self::Expr(expr) => write!(f, "$({})", EscapeHtml(expr)),
                Self::Text(text) => Display::fmt(&EscapeHtml(text), f),
                Self::Internal(i) => write!(f, "$__internal({i})"),
            }
        } else {
            match self {
                Self::OpeningTagStart(name) => write!(f, "<{name}"),
                Self::Attr(attr) => write!(f, "{attr}"),
                Self::OpeningTagEnd(s) => write!(f, "{s}"),
                Self::ClosingTag(name) => write!(f, "</{name}>"),
                Self::Var(name) => write!(f, "${name}"),
                Self::Expr(expr) => write!(f, "$({expr})"),
                Self::Text(text) => Display::fmt(text, f),
                Self::Internal(i) => write!(f, "$__internal({i})"),
            }
        }
    }
}

impl XmlFragment<'_> {
    /// For error reporting
    pub fn as_src_ptr(&self) -> *const u8 {
        match self {
            Self::Attr(attr) => attr.as_src_ptr(),
            Self::OpeningTagEnd(tag) => tag.as_src_ptr(),
            Self::OpeningTagStart(s)
            | Self::ClosingTag(s)
            | Self::Var(s)
            | Self::Expr(s)
            | Self::Text(s) => s.as_ptr(),
            Self::Internal(_) => null(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expected {
    ClosedTag,
    Word,
    StrLiteral,
    TemplateExpr,
    TemplateVarName,
    AttrName,
    Comment,
}

impl Display for Expected {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ClosedTag => write!(f, "expected `/>` or `>`"),
            Self::Word => write!(f, "expected a word terminated by whitespace, `/>` or `>`"),
            Self::StrLiteral => write!(f, "expected a string terminated by `\"`"),
            Self::TemplateExpr => write!(f, "expected an expression terminated by `)`"),
            Self::TemplateVarName => write!(f, "expected a variable name"),
            Self::AttrName => write!(f, "expected an attribute name"),
            Self::Comment => write!(f, "expected a comment terminated by `-->`"),
        }
    }
}

impl Error for Expected {}

impl From<Infallible> for Expected {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

type ParsingResult<'input, T = (), E = Expected> = shrimple_parser::ParsingResult<'input, T, E>;

#[allow(clippy::trivially_copy_pass_by_ref, /* reason="easier to pass through parsers" */)]
fn is_ident_char(ch: &char) -> bool {
    ch.is_alphanumeric() || "_-$".contains(*ch)
}

fn parse_word(input: &str) -> ParsingResult<&str> {
    parse_until(is_in([' ', '\n', '\t', '/', '>'])).expect(Expected::Word)(input)
}

fn parse_whitespace(input: &str) -> ParsingResult<&str, Infallible> {
    parse_while(char::is_ascii_whitespace)(input)
}

/// Recoverable if no initial double quote found
fn parse_string_literal(input: &str) -> ParsingResult<&str> {
    parse_char('"').then(parse_until_ex(eq('"')).expect(Expected::StrLiteral))(input)
}

/// Recoverable if no initial "$(" found
fn parse_template_expr(input: &str) -> ParsingResult<&str> {
    parse_char('$').then(parse_group('(', ')').narrow_reason(Expected::TemplateExpr))(input)
}

fn parse_template_var(input: &str) -> ParsingResult<&str> {
    parse_char('$')
        .then(parse_while(is_ident_char).filter(ne("")).expect(Expected::TemplateVarName))(input)
}

fn parse_attr_value(input: &str) -> ParsingResult<AttrValue> {
    parse_template_expr
        .map(AttrValue::Expr)
        .or(parse_template_var.map(AttrValue::Var))
        .or(parse_string_literal.map(AttrValue::Text))
        .or(parse_word.map(AttrValue::Text))(input)
}

fn parse_attr(input: &str) -> ParsingResult<Attr> {
    parse_whitespace
        .map(str::len)
        .skip(parse_while(is_ident_char))
        .expect(Expected::AttrName)
        .get_span()
        .add(parse_char('=').then(parse_attr_value).or_value(AttrValue::None))
        .map(from_tuple!(Attr { name_start, prefix_and_name, value }))(input)
}

/// The output is the element name, without the `<`.
/// Recoverable if no initial `<` is found.
fn parse_opening_tag_start(input: &str) -> ParsingResult<&str> {
    parse_char('<').then(parse_word)(input)
}

/// Any returned error is recoverable.
fn parse_opening_tag_end(input: &str) -> ParsingResult<OpeningTagEnd, Infallible> {
    parse_whitespace
        .maybe_skip(parse_char('/'))
        .skip(parse_char('>'))
        .get_span()
        .map(second)
        .map(OpeningTagEnd)(input)
}

/// Recoverable if no initial `<!--` is found.
fn parse_comment(input: &str) -> ParsingResult<&str> {
    parse_exact("<!--").then(parse_until_exact("-->").expect(Expected::Comment))(input)
}

fn parse_closing_tag(input: &str) -> ParsingResult<&str> {
    parse_exact("</")
        .then(parse_word)
        .maybe_skip(parse_whitespace)
        .skip(parse_char('>').expect(Expected::ClosedTag))(input)
}

fn parse_xml_fragment(input: &str) -> ParsingResult<XmlFragment> {
    parse_comment.repeat().then(
        parse_closing_tag
            .map(XmlFragment::ClosingTag)
            .or(parse_opening_tag_start.map(XmlFragment::OpeningTagStart))
            .or(parse_template_expr.map(XmlFragment::Expr))
            .or(parse_template_var.map(XmlFragment::Var))
            .or(parse_until(is_in(['$', '<'])).map(XmlFragment::Text).narrow_reason(Expected::Word))
            .or_map_rest(XmlFragment::Text),
    )(input)
}

fn parse_xml_fragment_in_opening_tag(input: &str) -> ParsingResult<XmlFragment> {
    parse_opening_tag_end.map(XmlFragment::OpeningTagEnd).or(parse_attr.map(XmlFragment::Attr))(
        input,
    )
}

pub struct ShrimpleParser {
    file: Asset,
    input: &'static str,
    parsing_attrs: bool,
    error: Result<(), FullParsingError<'static, Expected>>,
}

impl Iterator for ShrimpleParser {
    type Item = XmlFragment<'static>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error.is_err() || self.input.is_empty() {
            return None;
        }

        let res = if self.parsing_attrs {
            parse_xml_fragment_in_opening_tag(self.input)
        } else {
            parse_xml_fragment(self.input)
        };
        match res {
            Ok((input, res)) => {
                self.input = input;
                self.parsing_attrs =
                    matches!(res, XmlFragment::OpeningTagStart(_) | XmlFragment::Attr(_));
                Some(res)
            }
            Err(e) => {
                self.error = Err(e.with_src_loc(self.file.path, self.file.src().unwrap_or("")));
                None
            }
        }
    }
}

impl ShrimpleParser {
    pub const fn new(input: &'static str, file: Asset) -> Self {
        Self { input, file, parsing_attrs: false, error: Ok(()) }
    }

    pub const fn file(&self) -> &Asset {
        &self.file
    }

    /// returns
    pub const fn finish(self) -> Result<(), FullParsingError<'static, Expected>> {
        self.error
    }
}

pub fn url_scheme(url: &str) -> Option<&str> {
    url.split_once(':').map(first)
}
