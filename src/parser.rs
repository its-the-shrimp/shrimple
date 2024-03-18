use crate::{
    error::Locator,
    utils::{group, prefixed, surrounded, whitespace, ParserExt, Result},
};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, satisfy},
    combinator::map,
    multi::{many0_count, many1_count},
    sequence::{delimited, preceded},
    IResult, Parser,
};
use std::{
    fmt::{self, Debug, Display, Formatter, Write},
    marker::PhantomData,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => Ok(()),
            Self::Var(name) => write!(f, "${name}"),
            Self::Expr(expr) => write!(f, "$({expr})"),
            Self::Text(text) => Debug::fmt(text, f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Attr<'src> {
    pub name: &'src str,
    pub value: AttrValue<'src>,
}

impl Display for Attr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if matches!(self.value, AttrValue::None) {
            Display::fmt(self.name, f)
        } else {
            write!(f, "{}={}", self.name, self.value)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum XmlFragment<'src, Cmd = ()> {
    /// <tagname
    OpeningTagStart(&'src str),
    /// `key=value` or key
    Attr(Attr<'src>),
    /// `/>` or `>`
    OpeningTagEnd(&'src str),
    /// </tagname>
    ClosingTag(&'src str),
    /// $VARNAME
    Var(&'src str),
    /// $(LUA CODE)
    Expr(&'src str),
    /// any other text
    Text(&'src str),
    /// used by the evaluator
    Internal(Cmd),
}

impl<Cmd: Display> Display for XmlFragment<'_, Cmd> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::OpeningTagStart(name) => write!(f, "<{}", name),
            Self::Attr(attr) => write!(f, " {attr}"),
            Self::OpeningTagEnd(s) => if s.starts_with('/') {
                f.write_str(" />")
            } else {
                f.write_char('>')
            }
            Self::ClosingTag(name) => write!(f, "</{}>", name),
            Self::Var(name) => write!(f, "${}", name),
            Self::Expr(expr) => write!(f, "$({})", expr),
            Self::Text(text) => Display::fmt(text, f),
            Self::Internal(i) => write!(f, "$__internal({i})"),
        }
    }
}

impl<Cmd> XmlFragment<'_, Cmd> {
    /// For error reporting
    pub fn as_src_ptr(&self) -> *const u8 {
        match self {
            Self::OpeningTagStart(s)
            | Self::Attr(Attr { name: s, .. })
            | Self::OpeningTagEnd(s)
            | Self::ClosingTag(s)
            | Self::Var(s)
            | Self::Expr(s)
            | Self::Text(s) => s.as_ptr(),
            Self::Internal(_) => null(),
        }
    }
}

fn ident_char(input: &str) -> IResult<&str, char> {
    satisfy(|c: char| c.is_alphanumeric() || c == '_')(input)
}

fn word(input: &str) -> IResult<&str, &str> {
    is_not(" \t\n/>")(input)
}

fn prefix(input: &str) -> IResult<&str, char> {
    char('$').parse(input)
}

fn string_literal(input: &str) -> IResult<&str, &str> {
    delimited(char('"'), is_not("\""), char('"')).parse(input)
}

fn attr_name(input: &str) -> IResult<&str, &str> {
    ident_char.or(prefix).and(many0_count(ident_char)).recognize().parse(input)
}

fn attr_value(input: &str) -> IResult<&str, AttrValue> {
    alt((
        map(template_expr, AttrValue::Expr),
        map(template_var, AttrValue::Var),
        map(string_literal, AttrValue::Text),
        map(word, AttrValue::Text),
    ))(input)
}

fn attr(input: &str) -> IResult<&str, Attr> {
    attr_name
        .and(prefixed(char('='), attr_value).opt())
        .map(|(name, value)| Attr { name, value: value.unwrap_or_default() })
        .parse(input)
}

fn template_var(input: &str) -> IResult<&str, &str> {
    prefixed(prefix, many1_count(ident_char).recognize())(input)
}

fn template_expr(input: &str) -> IResult<&str, &str> {
    prefixed(prefix.peek(char('(')), group('(', ')'))(input)
}

/// the output is the element name
fn opening_tag_start(input: &str) -> IResult<&str, &str> {
    prefixed(char('<'), word)(input)
}

fn closing_tag(input: &str) -> IResult<&str, &str> {
    surrounded(tag("</"), word, char('>'))(input)
}

fn xml_fragment<Cmd>(input: &str) -> IResult<&str, XmlFragment<Cmd>> {
    alt((
        map(closing_tag, XmlFragment::ClosingTag),
        map(opening_tag_start, XmlFragment::OpeningTagStart),
        map(template_expr, XmlFragment::Expr),
        map(template_var, XmlFragment::Var),
        map(is_not("$<"), XmlFragment::Text),
    ))(input)
}

fn xml_fragment_in_opening_tag<Cmd>(input: &str) -> IResult<&str, XmlFragment<Cmd>> {
    preceded(
        whitespace.opt(),
        alt((
            map(tag("/>"), XmlFragment::OpeningTagEnd),
            map(tag(">"), XmlFragment::OpeningTagEnd),
            map(attr, XmlFragment::Attr),
        )),
    )(input)
}

#[must_use = "use the `finish` method to properly handle a potential parsing error"]
pub struct ShrimpleParser<'locator, 'input, Cmd> {
    locator: &'locator Locator<'input>,
    input: &'input str,
    parsing_attrs: bool,
    error: Result,
    _cmd: PhantomData<Cmd>,
}

impl<'locator, 'input, Cmd: Copy> Iterator for ShrimpleParser<'locator, 'input, Cmd> {
    type Item = XmlFragment<'input, Cmd>;

    fn next(&mut self) -> Option<Self::Item> {
        let &mut Self { locator, input, parsing_attrs, ref error, .. } = self;

        if error.is_err() {
            return None;
        }

        let res =
            if parsing_attrs { xml_fragment_in_opening_tag(input) } else { xml_fragment(input) };
        match res {
            Ok((input, res)) => {
                self.input = input;
                self.parsing_attrs = matches!(
                    res,
                    XmlFragment::OpeningTagStart(_) | XmlFragment::Attr(_)
                );
                Some(res)
            }
            Err(nom::Err::Error(nom::error::Error { input: "", .. })) => {
                // to preserve the address
                self.input = &input[input.len()..];
                None
            }
            Err(e) => {
                self.error = Err(locator.wrap_nom_error(e));
                None
            }
        }
    }
}

impl<'locator, 'input, Cmd> ShrimpleParser<'locator, 'input, Cmd> {
    pub fn new(input: &'input str, locator: &'locator Locator<'input>) -> Self {
        Self { input, locator, parsing_attrs: false, _cmd: PhantomData, error: Ok(()) }
    }

    pub fn locator(&self) -> &'locator Locator<'input> {
        self.locator
    }

    /// returns
    pub fn finish(self) -> Result {
        self.error
    }
}
