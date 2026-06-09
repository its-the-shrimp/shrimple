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
        cell::Cell,
        convert::Infallible,
        error::Error,
        fmt::{self, Debug, Display, Formatter},
        mem::take,
    },
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Attr {
    prefix_and_name: StrView,
    /// Safety: `self.name_start < self.prefix_and_name.len()`
    name_start: usize,
    pub value: Option<TextLexeme>,
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
    fn lex_attr_value(input: StrView) -> ParsingResult<TextLexeme> {
        any! {
            lex_template_expr.map(TextLexeme::Expr),
            lex_template_var.map(TextLexeme::Var),
            lex_string_literal.map(TextLexeme::Text),
            lex_word.map(TextLexeme::Text),
        }
        .parse(input)
    }

    fn lex(input: StrView) -> ParsingResult<Self> {
        lex_whitespace
            .map(|s| s.len())
            .skip(parse_while(is_ident_char).filter(|s: &StrView| !s.is_empty()))
            .expect(Expected::AttrName)
            .get_span()
            .add(parse('=').map_reason(|x| match x {}).then(Self::lex_attr_value).maybe())
            .map(from_tuple!(Attr { name_start, prefix_and_name, value }))
            .parse(input)
    }

    pub fn as_src_ptr(&self) -> *const u8 {
        self.prefix_and_name.as_ptr()
    }

    pub fn into_parts(self) -> (StrView, Option<TextLexeme>) {
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
    /// Any returned error is recoverable.
    fn lex(input: StrView) -> ParsingResult<Self, Infallible> {
        lex_whitespace
            .skip(parse('/'))
            .and(parse('>'))
            .get_span()
            .map(|(.., span)| Self(span))
            .parse(input)
    }

    pub fn as_src_ptr(&self) -> *const u8 {
        self.0.as_ptr().wrapping_add(self.0.len() - 1 - usize::from(self.0.starts_with("/>")))
    }

    pub fn is_self_closing(&self) -> bool {
        self.0.ends_with("/>")
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TextLexeme {
    /// $VARNAME
    Var(StrView),
    /// $(LUA CODE)
    Expr(StrView),
    /// any other text
    Text(StrView),
}

impl Display for TextLexeme {
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

impl TextLexeme {
    fn lex(input: StrView) -> ParsingResult<Self> {
        any! {
            lex_template_expr.map(TextLexeme::Expr),
            lex_template_var.map(TextLexeme::Var),
            parse_until('$').map(TextLexeme::Text).map_reason(|x| match x {}),
            else: TextLexeme::Text
        }
        .parse(input)
    }

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
pub enum Lexeme {
    /// <tagname
    OpeningTagStart(StrView),
    /// `key=value` or key
    Attr(Attr),
    /// `/>` or `>`
    OpeningTagEnd(OpeningTagEnd),
    /// </tagname>
    ClosingTag(StrView),
    /// $VARNAME / $(LUA CODE) / any other text
    Text(TextLexeme),
}

impl Display for Lexeme {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            match self {
                Self::OpeningTagStart(name) => write!(f, "&lt;{}", EscapeHtml(name)),
                Self::Attr(attr) => write!(f, "{attr:#}"),
                Self::OpeningTagEnd(s) => write!(f, "{s:#}"),
                Self::ClosingTag(name) => write!(f, "&lt;/{}&gt;", EscapeHtml(name)),
                Self::Text(textlike) => write!(f, "{textlike:#}"),
            }
        } else {
            match self {
                Self::OpeningTagStart(name) => write!(f, "<{name}"),
                Self::Attr(attr) => write!(f, "{attr}"),
                Self::OpeningTagEnd(s) => write!(f, "{s}"),
                Self::ClosingTag(name) => write!(f, "</{name}>"),
                Self::Text(textlike) => write!(f, "{textlike}"),
            }
        }
    }
}

impl Lexeme {
    fn lex(input: StrView) -> ParsingResult<Self> {
        lex_comment
            .repeat()
            .then(any! {
                lex_code_span.map(TextLexeme::Text).map(Self::Text),
                lex_closing_tag.map(Lexeme::ClosingTag),
                lex_opening_tag_start.map(Lexeme::OpeningTagStart),
                lex_template_expr.map(TextLexeme::Expr).map(Self::Text),
                lex_template_var.map(TextLexeme::Var).map(Self::Text),
                parse_until(is_in(['$', '<', '`'])).map(TextLexeme::Text).map(Self::Text)
                    .map_reason(|x| match x {}),
                else: |x| Self::Text(TextLexeme::Text(x))
            })
            .parse(input)
    }

    fn lex_in_opening_tag(input: StrView) -> ParsingResult<Self> {
        OpeningTagEnd::lex
            .map_reason(|x| match x {})
            .map(Self::OpeningTagEnd)
            .or(Attr::lex.map(Lexeme::Attr))
            .parse(input)
    }

    // TODO: do normal spans
    pub fn as_src_ptr(&self) -> *const u8 {
        match self {
            Self::Attr(attr) => attr.as_src_ptr(),
            Self::OpeningTagEnd(tag) => tag.as_src_ptr(),
            Self::Text(textlike) => textlike.as_src_ptr(),
            Self::OpeningTagStart(s) | Self::ClosingTag(s) => s.as_ptr(),
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

fn lex_word(input: StrView) -> ParsingResult<StrView> {
    parse_until(is_in([' ', '\n', '\t', '/', '>'])).expect(Expected::Word)(input)
}

fn lex_whitespace(input: StrView) -> ParsingResult<StrView, Infallible> {
    parse_while(char::is_ascii_whitespace)(input)
}

/// Recoverable if no initial double quote found
fn lex_string_literal(input: StrView) -> ParsingResult<StrView> {
    parse('"')
        .map_reason(|x| match x {})
        .then(parse_until_ex('"').expect(Expected::StrLiteral))
        .parse(input)
}

/// Recoverable if no initial "$(" found
fn lex_template_expr(input: StrView) -> ParsingResult<StrView> {
    parse('$')
        .map_reason(|x| match x {})
        .then(parse_group('(', ')').map_reason(|_| Expected::TemplateExpr))
        .parse(input)
}

fn lex_template_var(input: StrView) -> ParsingResult<StrView> {
    parse('$')
        .map_reason(|x| match x {})
        .then(
            parse_while(is_ident_char)
                .filter(|s: &StrView| !s.is_empty())
                .expect(Expected::TemplateVarName),
        )
        .parse(input)
}

fn lex_code_span(input: StrView) -> ParsingResult<StrView> {
    parse('`')
        .skip(parse_until_ex('`'))
        .get_span()
        .map(|(_, span)| span)
        .map_reason(|x| match x {})
        .parse(input)
}

/// The output is the element name, without the `<`.
/// Recoverable if no initial `<` is found.
fn lex_opening_tag_start(input: StrView) -> ParsingResult<StrView> {
    parse('<').map_reason(|x| match x {}).then(lex_word)(input)
}

/// Recoverable if no initial `<!--` is found.
fn lex_comment(input: StrView) -> ParsingResult<StrView> {
    parse("<!--")
        .map_reason(|x| match x {})
        .then(parse_until_ex("-->").expect(Expected::Comment))
        .parse(input)
}

fn lex_closing_tag(input: StrView) -> ParsingResult<StrView> {
    parse("</")
        .map_reason(|x| match x {})
        .then(lex_word)
        .skip(lex_whitespace.map_reason(|x| match x {}))
        .skip(parse('>').expect(Expected::ClosedTag))
        .parse(input)
}

/// Parses only text & yields [`XmlTextFragment`]s
pub struct TextLexer<'a> {
    asset: Asset,
    input: StrView,
    result: &'a Cell<Result>,
}

impl Iterator for TextLexer<'_> {
    type Item = TextLexeme;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input.is_empty() {
            return None;
        }

        if let Err(e) = self.result.replace(Ok(())) {
            self.result.set(Err(e));
            return None;
        }

        let input = take(&mut self.input);
        match TextLexeme::lex(input) {
            Ok((rest, res)) => {
                self.input = rest;
                Some(res)
            }
            Err(e) => {
                self.result.set(Err(wrap_parsing_error(&e, &self.asset)));
                None
            }
        }
    }
}

impl<'a> TextLexer<'a> {
    pub const fn new(input: StrView, asset: Asset, result: &'a Cell<Result>) -> Self {
        Self { asset, input, result }
    }
}

/// Parses text & HTML elements
pub struct Lexer<'a> {
    asset: Asset,
    input: StrView,
    parsing_attrs: bool,
    result: &'a Cell<Result>,
}

impl Iterator for Lexer<'_> {
    type Item = Lexeme;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input.is_empty() {
            return None;
        }

        if let Err(e) = self.result.replace(Ok(())) {
            self.result.set(Err(e));
            return None;
        }

        let input = take(&mut self.input);
        match if self.parsing_attrs {
            Lexeme::lex_in_opening_tag(input)
        } else {
            Lexeme::lex(input)
        } {
            Ok((rest, res)) => {
                self.input = rest;
                self.parsing_attrs = matches!(res, Lexeme::OpeningTagStart(_) | Lexeme::Attr(_));
                Some(res)
            }
            Err(e) => {
                self.result.set(Err(wrap_parsing_error(&e, &self.asset)));
                None
            }
        }
    }
}

impl<'a> Lexer<'a> {
    pub const fn new(input: StrView, asset: Asset, result: &'a Cell<Result>) -> Self {
        Self { input, asset, parsing_attrs: false, result }
    }
}

pub fn url_scheme(url: &str) -> Option<&str> {
    url.split_once("://").map(first)
}
