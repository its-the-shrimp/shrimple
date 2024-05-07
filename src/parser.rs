use crate::{
    asset::Asset, escape_html::EscapeHtml, utils::{first, group, prefixed, str_from_raw_parts, surrounded, whitespace, ParserExt, Result}
};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until1},
    character::complete::{char, satisfy},
    combinator::{map, recognize},
    multi::{many0_count, many1_count},
    sequence::delimited,
    IResult, Parser,
};
use std::{
    fmt::{self, Debug, Display, Formatter}, marker::PhantomData, mem::replace, ptr::null, 
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
            if !self.has_value() {
                Display::fmt(&EscapeHtml(self.prefix_and_name), f)
            } else {
                write!(f, "{}={:#}", EscapeHtml(self.prefix_and_name), self.value)
            }
        } else if !self.has_value() {
            Display::fmt(self.prefix_and_name, f)
        } else {
            write!(f, "{}={}", self.prefix_and_name, self.value)
        }
    }
}

impl<'src> Attr<'src> {
    pub fn has_value(&self) -> bool {
        !matches!(self.value, AttrValue::None)
    }

    pub fn as_src_ptr(&self) -> *const u8 {
        self.prefix_and_name.as_ptr()
    }

    pub fn name(&self) -> &'src str {
        unsafe {
            self.prefix_and_name.get_unchecked(self.name_start ..)
        }
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
        unsafe {
            self.0.as_ptr().add(self.0.len() - 1 - self.0.starts_with("/>") as usize)
        }
    }

    pub fn is_self_closing(&self) -> bool {
        self.0.ends_with("/>")
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum XmlFragment<'src, Cmd = ()> {
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
    Internal(Cmd),
}

impl<Cmd: Display> Display for XmlFragment<'_, Cmd> {
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

impl<Cmd> XmlFragment<'_, Cmd> {
    /// For error reporting
    pub fn as_src_ptr(&self) -> *const u8 {
        match self {
            Self::Attr(attr) => attr.as_src_ptr(),
            Self::OpeningTagEnd(tag) => tag.as_src_ptr(),
            | Self::OpeningTagStart(s)
            | Self::ClosingTag(s)
            | Self::Var(s)
            | Self::Expr(s)
            | Self::Text(s) => s.as_ptr(),
            Self::Internal(_) => null(),
        }
    }
}

fn ident_char(input: &str) -> IResult<&str, char> {
    satisfy(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(input)
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

fn attr_value(input: &str) -> IResult<&str, AttrValue> {
    alt((
        map(template_expr, AttrValue::Expr),
        map(template_var, AttrValue::Var),
        map(string_literal, AttrValue::Text),
        map(word, AttrValue::Text),
    ))(input)
}

fn attr(input: &str) -> IResult<&str, Attr> {
    let (input, (prefix_and_name, name_start)) = whitespace
        .and(ident_char.or(prefix))
        .and(many0_count(ident_char))
        .map(|((prefix, _), name_len): ((&str, char), usize)| unsafe {
            (str_from_raw_parts(prefix.as_ptr(), prefix.len() + 1 + name_len), prefix.len())
        })
        .parse(input)?;
    prefixed(char('='), attr_value).opt()
        .map(|v| Attr { prefix_and_name, name_start, value: v.unwrap_or_default() })
        .parse(input)
}

fn template_var(input: &str) -> IResult<&str, &str> {
    prefixed(prefix, recognize(many1_count(ident_char)))(input)
}

fn template_expr(input: &str) -> IResult<&str, &str> {
    prefixed(prefix.peek(char('(')), group('(', ')'))(input)
}

/// the output is the element name
fn opening_tag_start(input: &str) -> IResult<&str, &str> {
    prefixed(char('<'), word)(input)
}

fn opening_tag_end(input: &str) -> IResult<&str, OpeningTagEnd> {
    recognize(whitespace.and(char('/').opt()).and(char('>'))).map(OpeningTagEnd).parse(input)
}

fn comment(input: &str) -> IResult<&str, &str> {
    prefixed(tag("<!--"), take_until1("-->")).trim(tag("-->")).parse(input)
}

fn closing_tag(input: &str) -> IResult<&str, &str> {
    surrounded(tag("</"), word, char('>'))(input)
}

fn xml_fragment<Cmd>(input: &str) -> IResult<&str, XmlFragment<Cmd>> {
    let (input, _) = many0_count(comment)(input)?;
    alt((
        map(closing_tag, XmlFragment::ClosingTag),
        map(opening_tag_start, XmlFragment::OpeningTagStart),
        map(template_expr, XmlFragment::Expr),
        map(template_var, XmlFragment::Var),
        map(is_not("$<"), XmlFragment::Text),
    ))(input)
}

fn xml_fragment_in_opening_tag<Cmd>(input: &str) -> IResult<&str, XmlFragment<Cmd>> {
    alt((
        map(opening_tag_end, XmlFragment::OpeningTagEnd),
        map(attr, XmlFragment::Attr),
    ))(input)
}

#[must_use = "use the `finish` method to properly handle a potential parsing error"]
pub struct ShrimpleParser<Cmd> {
    file: Asset,
    input: &'static str,
    parsing_attrs: bool,
    error: Result,
    _cmd: PhantomData<Cmd>,
}

impl<Cmd: Copy> Iterator for ShrimpleParser<Cmd> {
    type Item = XmlFragment<'static, Cmd>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.error.is_err() {
            return None;
        }

        let res = if self.parsing_attrs {
            xml_fragment_in_opening_tag(self.input)
        } else {
            xml_fragment(self.input)
        };
        match res {
            Ok((input, res)) => {
                self.input = input;
                self.parsing_attrs =
                    matches!(res, XmlFragment::OpeningTagStart(_) | XmlFragment::Attr(_));
                Some(res)
            }
            Err(nom::Err::Error(nom::error::Error { input: input @ "", .. })) => {
                // to preserve the address
                self.input = input;
                None
            }
            Err(e) => {
                self.error = Err(self.file.wrap_nom_error(e));
                None
            }
        }
    }
}

impl<Cmd> ShrimpleParser<Cmd> {
    pub fn new(input: &'static str, file: Asset) -> Self {
        Self { input, file, parsing_attrs: false, _cmd: PhantomData, error: Ok(()) }
    }

    pub fn file(&self) -> &Asset {
        &self.file
    }

    /// returns
    pub fn finish(&mut self) -> Result {
        replace(&mut self.error, Ok(()))
    }
}

pub fn url_scheme(url: &str) -> Option<&str> {
    url.split_once(':').map(first)
}
