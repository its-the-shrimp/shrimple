use {
    crate::{asset::Asset, error::ExtraCtx, escape_html::EscapeHtml, utils::Result, view::StrView},
    shrimple_parser::{
        Input, Location, Parser as _, ParsingError, Pattern, any, from_tuple,
        parser::{group, many, one, until, until_ex},
        pattern::whitespace,
        seq,
        tuple::{first, second},
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
            lex_template_expr.map_out(TextLexeme::Expr),
            lex_template_var.map_out(TextLexeme::Var),
            lex_string_literal.map_out(TextLexeme::Text),
            lex_word.filter_fatal(Expected::AttrValue, |x| !x.is_empty()).map_out(TextLexeme::Text),
        }
        .parse(input)
    }

    fn lex(input: StrView) -> ParsingResult<Self> {
        lex_whitespace::<Expected>
            .map_out(|s| s.len())
            .skip(many(is_ident_char).filter(|s: &StrView| !s.is_empty()))
            .and_span()
            .add(one('=').then(Self::lex_attr_value).maybe())
            .map_out(from_tuple!(Attr { name_start, prefix_and_name, value }))
            .parse(input)
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
    fn lex<Reason>(input: StrView) -> ParsingResult<Self, Reason> {
        one(seq!(whitespace.many(), '/'.maybe(), whitespace.many(), '>')).map_out(Self).parse(input)
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
            lex_template_expr.map_out(TextLexeme::Expr),
            lex_template_var.map_out(TextLexeme::Var),
            until('$').map_out(TextLexeme::Text),
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
    const fn plain_text(text: StrView) -> Self {
        Self::Text(TextLexeme::Text(text))
    }

    const fn var(name: StrView) -> Self {
        Self::Text(TextLexeme::Var(name))
    }

    const fn expr(code: StrView) -> Self {
        Self::Text(TextLexeme::Expr(code))
    }

    fn lex(input: StrView) -> ParsingResult<Self> {
        lex_comment
            .repeat()
            .then(any! {
                lex_code_span.map_out(Self::plain_text),
                lex_closing_tag.map_out(Self::ClosingTag),
                lex_opening_tag_start.map_out(Self::OpeningTagStart),
                lex_template_expr.map_out(Self::expr),
                lex_template_var.map_out(Self::var),
                until(['$', '<', '`']).map_out(Self::plain_text),
                else: Self::plain_text
            })
            .parse(input)
    }

    fn lex_in_opening_tag(input: StrView) -> ParsingResult<Self> {
        OpeningTagEnd::lex::<Expected>
            .map_out(Self::OpeningTagEnd)
            .or(Attr::lex.map_out(Self::Attr))
            .parse(input)
    }

    pub fn into_text(self) -> StrView {
        match self {
            Self::ClosingTag(span) | Self::OpeningTagStart(span) => span,
            Self::Attr(attr) => attr.into_parts().0,
            Self::OpeningTagEnd(opening_tag_end) => opening_tag_end.0,
            Self::Text(l) => l.into_text(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expected {
    ClosedTag,
    StrLiteral,
    TemplateExpr,
    TemplateVarName,
    AttrValue,
    Comment,
}

impl Display for Expected {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ClosedTag => write!(f, "expected `/>` or `>`"),
            Self::StrLiteral => write!(f, "expected a string terminated by `\"`"),
            Self::TemplateExpr => write!(f, "expected an expression terminated by `)`"),
            Self::TemplateVarName => write!(f, "expected a variable name"),
            Self::AttrValue => write!(
                f,
                "expected a Lua variable, a Lua code block, a string enclosed in double quotes, or a word terminated by whitespace or `>`"
            ),
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

    let Some(loc) = asset.src().and_then(|src| Location::find(e.rest.as_ptr(), &src)) else {
        return res;
    };

    res.context(ExtraCtx(loc.with_path(asset.path.to_string())))
}

type ParsingResult<T = (), E = Expected> = shrimple_parser::ParsingResult<StrView, T, E>;

fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ":_-$".contains(ch)
}

fn lex_word<Reason>(input: StrView) -> ParsingResult<StrView, Reason> {
    until((whitespace, '>'))(input)
}

fn lex_whitespace<Reason>(input: StrView) -> ParsingResult<StrView, Reason> {
    many(whitespace)(input)
}

/// Recoverable if no initial double quote found
fn lex_string_literal(input: StrView) -> ParsingResult<StrView> {
    one('"').then(until_ex('"').or_reason(Expected::StrLiteral)).parse(input)
}

/// Recoverable if no initial "$(" found
fn lex_template_expr(input: StrView) -> ParsingResult<StrView> {
    one('$').then(group('(', ')').map_reason(|_| Expected::TemplateExpr)).parse(input)
}

fn lex_template_var(input: StrView) -> ParsingResult<StrView> {
    one('$')
        .then(
            many(is_ident_char)
                .filter_fatal(Expected::TemplateVarName, |s: &StrView| !s.is_empty()),
        )
        .parse(input)
}

fn lex_code_span(input: StrView) -> ParsingResult<StrView> {
    one('`').skip(until_ex('`')).and_span().map_out(second).parse(input)
}

/// The output is the element name, without the `<`.
/// Recoverable if no initial `<` is found.
fn lex_opening_tag_start(input: StrView) -> ParsingResult<StrView> {
    one('<').then(lex_word)(input)
}

/// Recoverable if no initial `<!--` is found.
fn lex_comment(input: StrView) -> ParsingResult<StrView> {
    one("<!--").then(until_ex("-->").or_reason(Expected::Comment)).parse(input)
}

fn lex_closing_tag(input: StrView) -> ParsingResult<StrView> {
    one("</")
        .then(lex_word)
        .skip(lex_whitespace)
        .skip(one('>').or_reason(Expected::ClosedTag))
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
