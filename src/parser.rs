use {
    crate::{
        asset::Asset,
        error::ExtraCtx,
        escape_html::EscapeHtml,
        utils::{Result, is_in},
        view::StrView,
    },
    shrimple_parser::{
        Input, Parser as _, ParsingError, any, from_tuple,
        pattern::{parse, parse_group, parse_until, parse_until_ex, parse_while},
        tuple::first,
        utils::{FullLocation, PathLike, locate},
    },
    std::{
        convert::Infallible,
        error::Error,
        fmt::{self, Debug, Display, Formatter},
        mem::take,
        ptr::null,
    },
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Attr {
    prefix_and_name: StrView,
    /// Safety: `self.name_start < self.prefix_and_name.len()`
    name_start: usize,
    pub value: Option<XmlTextFragment>,
}

impl Display for Attr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            if let Some(value) = &self.value {
                write!(f, "{}={value:#}", EscapeHtml(&self.prefix_and_name))
            } else {
                Display::fmt(&EscapeHtml(&self.prefix_and_name), f)
            }
        } else if let Some(value) = &self.value {
            write!(f, "{}={value}", self.prefix_and_name)
        } else {
            Display::fmt(&self.prefix_and_name, f)
        }
    }
}

impl Attr {
    pub const fn has_value(&self) -> bool {
        self.value.is_some()
    }

    pub fn as_src_ptr(&self) -> *const u8 {
        self.prefix_and_name.as_ptr()
    }

    pub fn name(&self) -> &str {
        &self.prefix_and_name[self.name_start..]
    }

    pub fn into_parts(self) -> (StrView, Option<XmlTextFragment>) {
        (self.prefix_and_name.after(self.name_start), self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
// Safety invariant: `!self.0.is_empty()`
pub struct OpeningTagEnd(StrView);

impl Display for OpeningTagEnd {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl OpeningTagEnd {
    /// An opening tag end span that doesn't point to any source code, but that spans `/>`
    pub fn default_self_closing() -> Self {
        Self("/>".into())
    }

    /// An opening tag end span that doesn't point to any source code, but that spans `>`
    pub fn default_with_children() -> Self {
        Self(">".into())
    }

    pub fn as_src_ptr(&self) -> *const u8 {
        self.0.as_ptr().wrapping_add(self.0.len() - 1 - usize::from(self.0.starts_with("/>")))
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
pub enum XmlTextFragment {
    /// $VARNAME
    Var(StrView),
    /// $(LUA CODE)
    Expr(StrView),
    /// any other text
    Text(StrView),
}

impl Display for XmlTextFragment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Self::Var(name) => write!(f, "${}", EscapeHtml(name)),
                Self::Expr(expr) => write!(f, "$({})", EscapeHtml(expr)),
                Self::Text(text) => Display::fmt(&EscapeHtml(text), f),
            }
        } else {
            match self {
                Self::Var(name) => write!(f, "${name}"),
                Self::Expr(expr) => write!(f, "$({expr})"),
                Self::Text(text) => Display::fmt(text, f),
            }
        }
    }
}

impl XmlTextFragment {
    pub const fn as_text(&self) -> &StrView {
        match self {
            Self::Var(view) | Self::Expr(view) | Self::Text(view) => view,
        }
    }

    pub fn into_text(self) -> StrView {
        match self {
            Self::Var(view) | Self::Expr(view) | Self::Text(view) => view,
        }
    }

    pub fn as_src_ptr(&self) -> *const u8 {
        match self {
            Self::Var(s) | Self::Expr(s) | Self::Text(s) => s.as_ptr(),
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
    /// $VARNAME / $(LUA CODE) / any other text
    Text(XmlTextFragment),
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
                Self::Text(textlike) => write!(f, "{textlike:#}"),
                Self::Internal(i) => write!(f, "$__internal({i})"),
            }
        } else {
            match self {
                Self::OpeningTagStart(name) => write!(f, "<{name}"),
                Self::Attr(attr) => write!(f, "{attr}"),
                Self::OpeningTagEnd(s) => write!(f, "{s}"),
                Self::ClosingTag(name) => write!(f, "</{name}>"),
                Self::Text(textlike) => write!(f, "{textlike}"),
                Self::Internal(i) => write!(f, "$__internal({i})"),
            }
        }
    }
}

impl XmlFragment {
    // TODO: do normal spans
    /// For error reporting
    pub fn as_src_ptr(&self) -> *const u8 {
        match self {
            Self::Attr(attr) => attr.as_src_ptr(),
            Self::OpeningTagEnd(tag) => tag.as_src_ptr(),
            Self::Text(textlike) => textlike.as_src_ptr(),
            Self::OpeningTagStart(s) | Self::ClosingTag(s) => s.as_ptr(),
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

fn wrap_parsing_error(e: &ParsingError<impl Input, Expected>, asset: &Asset) -> anyhow::Error {
    let res = match e.reason {
        Some(reason) => anyhow::Error::new(reason),
        None => anyhow::Error::msg("bug: parsing error without reason"),
    };

    let Some(loc) = asset.template_src().and_then(|src| locate(e.rest.as_ptr(), src)) else {
        return res;
    };

    res.context(ExtraCtx(FullLocation { path: asset.path.to_string().into_path_bytes(), loc }))
}

type ParsingResult<T = (), E = Expected> = shrimple_parser::ParsingResult<StrView, T, E>;

#[expect(clippy::trivially_copy_pass_by_ref, reason = "easier to pass through parsers")]
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
        .then(
            parse_while(is_ident_char)
                .filter(|s: &StrView| !s.is_empty())
                .expect(Expected::TemplateVarName),
        )
        .parse(input)
}

fn parse_xml_text_fragment(input: StrView) -> ParsingResult<XmlTextFragment> {
    any! {
        parse_template_expr.map(XmlTextFragment::Expr),
        parse_template_var.map(XmlTextFragment::Var),
        parse_until('$').map(XmlTextFragment::Text).map_reason(|x| match x {}),
        else: XmlTextFragment::Text
    }
    .parse(input)
}

fn parse_attr_value(input: StrView) -> ParsingResult<XmlTextFragment> {
    any! {
        parse_template_expr.map(XmlTextFragment::Expr),
        parse_template_var.map(XmlTextFragment::Var),
        parse_string_literal.map(XmlTextFragment::Text),
        parse_word.map(XmlTextFragment::Text),
    }
    .parse(input)
}

fn parse_attr(input: StrView) -> ParsingResult<Attr> {
    parse_whitespace
        .map(|s| s.len())
        .skip(parse_while(is_ident_char).filter(|s: &StrView| !s.is_empty()))
        .expect(Expected::AttrName)
        .get_span()
        .add(parse('=').map_reason(|x| match x {}).then(parse_attr_value).maybe())
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
            parse_template_expr.map(XmlTextFragment::Expr).map(XmlFragment::Text),
            parse_template_var.map(XmlTextFragment::Var).map(XmlFragment::Text),
            parse_until(is_in(['$', '<'])).map(XmlTextFragment::Text).map(XmlFragment::Text)
                .map_reason(|x| match x {}),
            else: |x| XmlFragment::Text(XmlTextFragment::Text(x))
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

/// Parses only text & yields [`XmlTextFragment`]s
pub struct TextParser {
    asset: Asset,
    input: StrView,
    error: Result,
}

impl Iterator for TextParser {
    type Item = XmlTextFragment;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error.is_err() || self.input.is_empty() {
            return None;
        }

        match parse_xml_text_fragment(take(&mut self.input)) {
            Ok((rest, res)) => {
                self.input = rest;
                Some(res)
            }
            Err(e) => {
                self.error = Err(wrap_parsing_error(&e, &self.asset));
                None
            }
        }
    }
}

impl TextParser {
    pub const fn new(input: StrView, asset: Asset) -> Self {
        Self { input, asset, error: Ok(()) }
    }

    pub const fn asset(&self) -> &Asset {
        &self.asset
    }

    /// returns
    pub fn finish(self) -> Result {
        self.error
    }
}

/// Parses text & HTML elements
pub struct Parser {
    asset: Asset,
    input: StrView,
    parsing_attrs: bool,
    error: Result,
}

impl Iterator for Parser {
    type Item = XmlFragment;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error.is_err() || self.input.is_empty() {
            return None;
        }

        let input = take(&mut self.input);
        match if self.parsing_attrs {
            parse_xml_fragment_in_opening_tag(input)
        } else {
            parse_xml_fragment(input)
        } {
            Ok((rest, res)) => {
                self.input = rest;
                self.parsing_attrs =
                    matches!(res, XmlFragment::OpeningTagStart(_) | XmlFragment::Attr(_));
                Some(res)
            }
            Err(e) => {
                self.error = Err(wrap_parsing_error(&e, &self.asset));
                None
            }
        }
    }
}

impl Parser {
    pub const fn new(input: StrView, asset: Asset) -> Self {
        Self { input, asset, parsing_attrs: false, error: Ok(()) }
    }

    pub const fn asset(&self) -> &Asset {
        &self.asset
    }

    pub fn finish(self) -> Result {
        self.error
    }
}

pub fn url_scheme(url: &str) -> Option<&str> {
    url.split_once("://").map(first)
}
