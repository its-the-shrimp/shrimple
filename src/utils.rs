use nom::character::complete::{char, satisfy};
use nom::combinator::{cut, opt, peek, recognize};
use nom::error::{Error as NomError, ErrorKind as NomErrorKind, ParseError};
use nom::multi::many1_count;
use nom::sequence::{delimited, preceded};
use nom::{AsChar, IResult, InputIter, InputLength};
use nom::{Offset, Parser, Slice};
use core::slice;
use std::cmp::Ordering;
use std::ffi::OsStr;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::mem::transmute;
use std::ops::{RangeFrom, RangeTo};
use std::ptr::copy_nonoverlapping;
use std::str::from_utf8_unchecked;

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
        self.and(other).map(first)
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

pub const fn os_str(x: &str) -> &OsStr {
    unsafe { transmute(x) }
}

pub trait OptionExt<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E>;
}

impl<T> OptionExt<T> for Option<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E> {
        self.map(f).transpose()
    }
}

pub fn first<T1, T2>(x: (T1, T2)) -> T1 {
    x.0
}

pub unsafe fn assume_static<T>(x: &T) -> &'static T {
    transmute(x)
}

pub unsafe fn assume_static_mut<T>(x: &mut T) -> &'static mut T {
    transmute(x)
}

/// A by-value string of at most 15 bytes, making the struct the same size as a `&str`
#[derive(Clone, Copy)]
pub struct ShortStr {
    // TODO: think of ways to remove this by implementing the struct as a C-like string
    // safety invariant: 
    len: u8,
    buf: [u8; Self::CAP],
}

impl Debug for ShortStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl Display for ShortStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl PartialEq for ShortStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl Eq for ShortStr {}

impl PartialOrd for ShortStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ShortStr {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl AsRef<str> for ShortStr {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Default for ShortStr {
    fn default() -> Self {
        Self { len: 0, buf: [0; Self::CAP] }
    }
}

impl Write for ShortStr {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if self.len() + s.len() > Self::CAP {
            return Err(fmt::Error);
        }
        unsafe {
            let rest = self.buf.as_mut_ptr().add(self.len());
            copy_nonoverlapping(s.as_ptr(), rest, s.len());
            self.len += s.len() as u8;
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! short_str {
    ($str:expr) => {
        {
            const RES: ShortStr = match ShortStr::new($str) {
                Some(x) => x,
                None => panic!("constructing a short string"),
            };
            RES
        }
    };
}

impl ShortStr {
    pub const CAP: usize = 15;

    /// returns `None` if `src`'s length is over 15
    pub const fn new(src: &str) -> Option<Self> {
        let len = src.len();
        if len > Self::CAP {return None}
        let mut buf = [0; Self::CAP];
        let mut i = 0;
        let mut ptr = src.as_ptr();
        while i < len {
            unsafe {
                buf[i] = *ptr;
                ptr = ptr.add(1);
            }
            i += 1;
        }
        Some(Self {len: len as u8, buf})
    }

    pub const fn len(&self) -> usize {
        self.len as usize
    }

    #[inline(always)]
    pub const fn as_str(&self) -> &str {
        unsafe {from_utf8_unchecked(slice::from_raw_parts(self.buf.as_ptr(), self.len as usize))}
    }
}
