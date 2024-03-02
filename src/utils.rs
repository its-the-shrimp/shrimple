use std::ops::{RangeTo, RangeFrom};

use nom::{IResult, InputIter, AsChar, InputLength};
use nom::character::complete::{char, satisfy};
use nom::error::{ParseError, ErrorKind as NomErrorKind, Error as NomError};
use nom::multi::{many1_count, many0_count};
use nom::sequence::preceded;
use nom::{Parser, Slice, Offset};
use nom::combinator::{recognize, cut, opt, peek};

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

    /// `preceded()` as a method
    fn and_replace<O2>(self, next: impl Parser<I, O2, NomError<I>>) -> impl Parser<I, O2, NomError<I>> {
        preceded(self, next)
    }

    /// turns a recoverable error into a success value with `None` as the output
    fn opt(self) -> impl Parser<I, Option<O>, NomError<I>>
    where
        I: Clone,
    {
        opt(self)
    }

    /// on recoverable error: output is `true`, on success: output is `false`
    fn failed(self) -> impl Parser<I, bool, NomError<I>>
    where
        I: Clone,
    {
        self.opt().map(|r| r.is_none())
    }

    fn cut(self) -> impl Parser<I, O, NomError<I>> {
        cut(self)
    }

    fn trim_whitespace(self) -> impl Parser<I, O, NomError<I>>
    where
        I: Slice<RangeFrom<usize>> + InputIter + Clone + InputLength,
        <I as InputIter>::Item: AsChar,
    {
        self.trim(many0_count(satisfy(|c: char| c.is_ascii_whitespace())))
    }

    fn peek<P>(self, parser: impl Parser<I, P, NomError<I>>) -> impl Parser<I, O, NomError<I>>
    where
        I: Clone,
    {
        self.trim(peek(parser))
    }
}

impl<I, O, T: Parser<I, O, NomError<I>>> ParserExt<I, O> for T {}

pub fn eq<T: Eq + ?Sized>(second: &T) -> impl Fn(&T) -> bool + '_ {
    move |x| x == second
}

pub fn whitespace<I>(input: I) -> IResult<I, ()>
where
    I: Slice<RangeFrom<usize>> + InputIter + Clone + InputLength,
    <I as InputIter>::Item: AsChar,
{
    many1_count(satisfy(|c: char| c.is_ascii_whitespace())).map(drop).parse(input)
}

pub fn group(open: char, close: char) -> impl Fn(&str) -> IResult<&str, &str> {
    move |input| {
        let (src, _) = char(open)(input)?;
        let mut depth = 0usize;
        for (at, ch) in src.char_indices() {
            if ch == close {
                if depth == 0 {
                    let (res, src) = src.split_at(at);
                    return Ok((&src[1..], res))
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
    second: impl Parser<I, O, E>
) -> impl FnMut(I) -> IResult<I, O, E> {
    preceded(first, cut(second))
}
