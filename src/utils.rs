use std::fmt::{self, Debug, Display, Formatter};
use std::ops::{RangeFrom, RangeTo};
use nom::character::complete::{char, satisfy};
use nom::combinator::{cut, opt, peek, recognize};
use nom::error::{Error as NomError, ErrorKind as NomErrorKind, ParseError};
use nom::multi::many1_count;
use nom::sequence::{delimited, preceded};
use nom::{AsChar, IResult, InputIter, InputLength};
use nom::{Offset, Parser, Slice};

pub type Result<T = (), E = anyhow::Error> = std::result::Result<T, E>;

pub trait ParserExt<I, O>: Sized + Parser<I, O, NomError<I>> {
    /// `recognize()` as a method
    fn recognize(self) -> impl Parser<I, I, NomError<I>>
    where
        I: Clone + Offset + Slice<RangeTo<usize>>,
    {
        recognize(self)
    }

    /// equialent to `.and(next).map(|(prev, _new)| prev)`
    fn trim<O2>(self, other: impl Parser<I, O2, NomError<I>>) -> impl Parser<I, O, NomError<I>> {
        self.and(other).map(|(prev, _)| prev)
    }

    /// turns a recoverable error into a success value with `None` as the output
    fn opt(self) -> impl Parser<I, Option<O>, NomError<I>>
    where
        I: Clone,
    {
        opt(self)
    }

    fn peek<P>(self, parser: impl Parser<I, P, NomError<I>>) -> impl Parser<I, O, NomError<I>>
    where
        I: Clone,
    {
        self.trim(peek(parser))
    }
}

impl<I, O, T: Parser<I, O, NomError<I>>> ParserExt<I, O> for T {}

pub fn whitespace<I>(input: I) -> IResult<I, I>
where
    I: Slice<RangeTo<usize>> + Slice<RangeFrom<usize>> + InputIter + Clone + InputLength + Offset,
    <I as InputIter>::Item: AsChar,
{
    many1_count(satisfy(|c: char| c.is_ascii_whitespace())).recognize().parse(input)
}

pub fn group(open: char, close: char) -> impl Fn(&str) -> IResult<&str, &str> {
    move |input| {
        let (src, _) = char(open)(input)?;
        let mut depth = 0usize;
        for (at, ch) in src.char_indices() {
            if ch == close {
                if depth == 0 {
                    let (res, src) = src.split_at(at);
                    return Ok((&src[1..], res));
                }
                depth -= 1
            } else if ch == open {
                depth += 1
            }
        }
        Err(nom::Err::Failure(NomError::new(src, NomErrorKind::Many1Count)))
    }
}

/// equivalent to `preceded(first, cut(second))`
pub fn prefixed<I, P, O, E: ParseError<I>>(
    first: impl Parser<I, P, E>,
    second: impl Parser<I, O, E>,
) -> impl FnMut(I) -> IResult<I, O, E> {
    preceded(first, cut(second))
}

/// equivalent to `delimited(prefix, cut(main), cut(postfix))`
pub fn surrounded<I, Prefix, O, Postfix, E: ParseError<I>>(
    first: impl Parser<I, Prefix, E>,
    main: impl Parser<I, O, E>,
    postfix: impl Parser<I, Postfix, E>,
) -> impl FnMut(I) -> IResult<I, O, E> {
    delimited(first, cut(main), cut(postfix))
}

/// Exists to compare/print strings as if they had an additional char `P` before them, without
/// allocating
#[repr(transparent)]
#[derive(Debug)]
pub struct Prefixed<const P: char, T>(pub T);

impl<const P: char, T: PartialEq<str>> PartialEq<str> for Prefixed<P, T> {
    fn eq(&self, other: &str) -> bool {
        other.strip_prefix(P).is_some_and(|rest| self.0 == *rest)
    }
}

impl<'str, const P: char, T: PartialEq<&'str str>> PartialEq<&'str str> for Prefixed<P, T> {
    fn eq(&self, other: &&'str str) -> bool {
        other.strip_prefix(P).is_some_and(|rest| self.0 == rest)
    }
}

impl<const P: char, T: Display> Display for Prefixed<P, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&P, f)?;
        Display::fmt(&self.0, f)
    }
}

pub fn default<T: Default>() -> T {
    T::default()
}
