use shrimple_parser::{
    any,
    from_tuple,
    pattern::{parse, parse_group, parse_until, parse_until_ex, parse_while},
    tuple::first,
    FullParsingError,
    Input,
    Parser,
};

use crate::{
    asset::Asset,
    escape_html::EscapeHtml,
    utils::{is_in, Result},
    view::StrView,
};
use std::{
    convert::Infallible, error::Error, fmt::{self, Debug, Display, Formatter}, mem::take, ptr::null
};

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub enum AttrValue {
    /// No value
    #[default]
    None,
    /// Without the `$`
    Var(StrView),
    /// Without the `$()`
    Expr(StrView),
    /// String literal or a word terminated by whitespace, `/`, `>`
    Text(StrView),
}

impl Display for AttrValue {
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

impl AttrValue {
    pub const fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Attr {
    prefix_and_name: StrView,
    /// Safety: `self.name_start < self.prefix_and_name.len()`
    name_start: usize,
    pub value: AttrValue,
}

impl Display for Attr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            if self.has_value() {
                write!(f, "{}={:#}", EscapeHtml(&self.prefix_and_name), self.value)
            } else {
                Display::fmt(&EscapeHtml(&self.prefix_and_name), f)
            }
        } else if self.has_value() {
            write!(f, "{}={}", self.prefix_and_name, self.value)
        } else {
            Display::fmt(&self.prefix_and_name, f)
        }
    }
}

impl Attr {
    pub const fn has_value(&self) -> bool {
        !matches!(self.value, AttrValue::None)
    }

    pub fn as_src_ptr(&self) -> *const u8 {
        self.prefix_and_name.as_ptr()
    }

    pub fn name(&self) -> &str {
        &self.prefix_and_name[self.name_start..]
    }

    pub fn into_parts(self) -> (StrView, AttrValue) {
        (self.prefix_and_name.after(self.name_start), self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
// Safety invariant: `!self.0.is_empty()`
pub struct OpeningTagEnd(StrView);

impl Display for OpeningTagEnd {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            Display::fmt(&EscapeHtml(&self.0), f)
        } else {
            Display::fmt(&self.0, f)
        }
    }
}

impl OpeningTagEnd {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum XmlFragment {
    /// <tagname
    OpeningTagStart(StrView),
    /// `key=value` or key
    Attr(Attr),
    /// `/>` or `>`
    OpeningTagEnd(OpeningTagEnd),
    /// </tagname>
    ClosingTag(StrView),
    /// $VARNAME
    Var(StrView),
    /// $(LUA CODE)
    Expr(StrView),
    /// any other text
    Text(StrView),
    /// used by the evaluator
    Internal(EvalCmd),
}

impl Display for XmlFragment {
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

impl XmlFragment {
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

type ParsingResult<T = (), E = Expected> = shrimple_parser::ParsingResult<StrView, T, E>;

#[expect(clippy::trivially_copy_pass_by_ref, reason="easier to pass through parsers")]
fn is_ident_char(ch: &char) -> bool {
    ch.is_alphanumeric() || ":_-$".contains(*ch)
}

fn parse_word(input: StrView) -> ParsingResult<StrView> {
    parse_until(is_in([' ', '\n', '\t', '/', '>'])).expect(Expected::Word)(input)
}

fn parse_whitespace(input: StrView) -> ParsingResult<StrView, Infallible> {
    parse_while(char::is_ascii_whitespace)(input)
}

/// Recoverable if no initial double quote found
fn parse_string_literal(input: StrView) -> ParsingResult<StrView> {
    parse('"')
        .map_reason(|x| match x {})
        .then(parse_until_ex('"').expect(Expected::StrLiteral))
        .parse(input)
}

/// Recoverable if no initial "$(" found
fn parse_template_expr(input: StrView) -> ParsingResult<StrView> {
    parse('$')
        .map_reason(|x| match x {})
        .then(parse_group('(', ')').map_reason(|_| Expected::TemplateExpr))
        .parse(input)
}

fn parse_template_var(input: StrView) -> ParsingResult<StrView> {
    parse('$')
        .map_reason(|x| match x {})
        .then(parse_while(is_ident_char) 
            .filter(|s: &StrView| !s.is_empty())
            .expect(Expected::TemplateVarName))
        .parse(input) 
}

fn parse_attr_value(input: StrView) -> ParsingResult<AttrValue> {
    any! {
        parse_template_expr.map(AttrValue::Expr),
        parse_template_var.map(AttrValue::Var),
        parse_string_literal.map(AttrValue::Text),
        parse_word.map(AttrValue::Text),
    }.parse(input)
}

fn parse_attr(input: StrView) -> ParsingResult<Attr> {
    parse_whitespace
        .map(|s| s.len())
        .skip(parse_while(is_ident_char).filter(|s: &StrView| !s.is_empty()))
        .expect(Expected::AttrName)
        .get_span()
        .add(parse('=')
            .map_reason(|x| match x {})
            .then(parse_attr_value)
            .or_value(AttrValue::None))
        .map(from_tuple!(Attr { name_start, prefix_and_name, value }))
        .parse(input)
}

/// The output is the element name, without the `<`.
/// Recoverable if no initial `<` is found.
fn parse_opening_tag_start(input: StrView) -> ParsingResult<StrView> {
    parse('<').map_reason(|x| match x {}).then(parse_word)(input)
}

/// Any returned error is recoverable.
fn parse_opening_tag_end(input: StrView) -> ParsingResult<OpeningTagEnd, Infallible> {
    parse_whitespace
        .skip(parse('/'))
        .and(parse('>'))
        .get_span()
        .map(|(.., span)| OpeningTagEnd(span))
        .parse(input)
}

/// Recoverable if no initial `<!--` is found.
fn parse_comment(input: StrView) -> ParsingResult<StrView> {
    parse("<!--")
        .map_reason(|x| match x {})
        .then(parse_until_ex("-->").expect(Expected::Comment))
        .parse(input)
}

fn parse_closing_tag(input: StrView) -> ParsingResult<StrView> {
    parse("</")
        .map_reason(|x| match x {})
        .then(parse_word)
        .skip(parse_whitespace.map_reason(|x| match x {}))
        .skip(parse('>').expect(Expected::ClosedTag))
        .parse(input)
}

fn parse_xml_fragment(input: StrView) -> ParsingResult<XmlFragment> {
    parse_comment
        .repeat()
        .then(any! {
            parse_closing_tag.map(XmlFragment::ClosingTag),
            parse_opening_tag_start.map(XmlFragment::OpeningTagStart),
            parse_template_expr.map(XmlFragment::Expr),
            parse_template_var.map(XmlFragment::Var),
            parse_until(is_in(['$', '<'])).map(XmlFragment::Text).map_reason(|x| match x {}),
            else: XmlFragment::Text
        })
        .parse(input)
}

fn parse_xml_fragment_in_opening_tag(input: StrView) -> ParsingResult<XmlFragment> {
    parse_opening_tag_end
        .map_reason(|x| match x {})
        .map(XmlFragment::OpeningTagEnd)
        .or(parse_attr.map(XmlFragment::Attr))
        .parse(input)
}

#[derive(Clone)]
pub struct ShrimpleParser {
    file: Asset,
    input: StrView,
    parsing_attrs: bool,
    error: Result<(), FullParsingError<'static, Expected>>,
}

impl Iterator for ShrimpleParser {
    type Item = XmlFragment;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error.is_err() || self.input.is_empty() {
            return None;
        }

        let input = take(&mut self.input);
        let res = if self.parsing_attrs {
            parse_xml_fragment_in_opening_tag(input)
        } else {
            parse_xml_fragment(input)
        };
        match res {
            Ok((input, res)) => {
                self.input = input;
                self.parsing_attrs =
                    matches!(res, XmlFragment::OpeningTagStart(_) | XmlFragment::Attr(_));
                Some(res)
            }
            Err(e) => {
                self.error = Err(e.with_src_loc(
                    self.file.path.to_path_buf(),
                    &self.file.src().unwrap_or_default(),
                ));
                None
            }
        }
    }
}

impl ShrimpleParser {
    pub const fn new(input: StrView, file: Asset) -> Self {
        Self { input, file, parsing_attrs: false, error: Ok(()) }
    }

    pub const fn file(&self) -> &Asset {
        &self.file
    }

    /// returns
    pub fn finish(self) -> Result<(), FullParsingError<'static, Expected>> {
        self.error
    }
}

pub fn url_scheme(url: &str) -> Option<&str> {
    url.split_once("://").map(first)
}
