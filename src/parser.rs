use std::{error::Error, path::Path};
use crate::error::ErrorWithSource;
use nom::{
    IResult,
    character::complete::{char, satisfy},
    Parser,
    combinator::map,
    multi::{many0_count, separated_list0, many1_count, many1},
    bytes::complete::{is_not, tag},
    sequence::delimited, branch::alt,
};
use crate::utils::{ParserExt, prefixed, whitespace, group};

#[derive(Debug, PartialEq, Eq, Default)]
pub enum AttrValue<'src> {
    /// No value
    #[default]
    None,
    /// Without the quotes
    String(&'src str),
    /// Without the `$`
    Var(&'src str),
    /// Without the `$()`
    Expr(&'src str),
    /// Anything else, just a word termimated by whitespace, `/` or `>`
    Text(&'src str),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Attr<'src> {
    pub name: &'src str,
    pub value: AttrValue<'src>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Element<'src> {
    pub name: &'src str,
    pub attrs: Vec<Attr<'src>>,
    pub has_children: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum XmlFragment<'src> {
    ElementStart(Element<'src>),
    ElementEnd(&'src str),
    Var(&'src str),
    Expr(&'src str),
    Text(&'src str),
}

fn ident_char(input: &str) -> IResult<&str, char> {
    satisfy(|c: char| c.is_alphanumeric() || c == '_')(input)
}

fn ident(input: &str) -> IResult<&str, &str> {
    ident_char.or(char('$')).and(many0_count(ident_char)).recognize().parse(input)
}

fn word(input: &str) -> IResult<&str, &str> {
    is_not(" \t\n/>")(input)
}

fn string_literal(input: &str) -> IResult<&str, &str> {
    delimited(char('"'), is_not("\""), char('"')).parse(input)
}

fn attr_value(input: &str) -> IResult<&str, AttrValue> {
    map(template_expr, AttrValue::Expr)
        .or(map(template_var, AttrValue::Var))
        .or(map(string_literal, AttrValue::String))
        .or(map(word, AttrValue::Text))
        .parse(input)
}

fn kv_pair(input: &str) -> IResult<&str, Attr> {
    ident
        .and(prefixed(char('='), attr_value).opt())
        .map(|(name, value)| Attr { name, value: value.unwrap_or_default() })
        .parse(input)
}

fn template_var(input: &str) -> IResult<&str, &str> {
    prefixed(char('$'), many1_count(ident_char).recognize())(input)
}

fn template_expr(input: &str) -> IResult<&str, &str> {
    prefixed(char('$').peek(char('(')), group('(', ')'))(input)
}

fn element_start(input: &str) -> IResult<&str, Element> {
    prefixed(
        char('<'),
        word
            .trim_whitespace()
            .and(separated_list0(whitespace, kv_pair))
            .trim_whitespace()
            .and(char('/').failed())
            .trim(char('>'))
            .map(|((name, attrs), has_children)| Element { name, attrs, has_children })
    )(input)
}

fn element_end(input: &str) -> IResult<&str, &str> {
    prefixed(tag("</"), word.trim(char('>')))(input)
}

fn xml_fragment(src: &str) -> IResult<&str, XmlFragment> {
    alt((
        map(element_end,   XmlFragment::ElementEnd),
        map(element_start, XmlFragment::ElementStart),
        map(template_expr, XmlFragment::Expr),
        map(template_var,  XmlFragment::Var),
        map(is_not("$<"),  XmlFragment::Text),
    ))
    .parse(src)
}

pub fn parse_file<'src>(name: &Path, src: &'src str) -> Result<Vec<XmlFragment<'src>>, impl Error> {
    use nom::Err::{Error as Errór, Incomplete, Failure};
    match many1(xml_fragment).parse(src) {
        Ok((_, res)) => Ok(res),
        Err(Incomplete(_)) => unreachable!("the parser isn't streaming"),
        Err(Errór(e) | Failure(e)) => Err(ErrorWithSource::from_nom_error(name, src, e)),
    }
}
