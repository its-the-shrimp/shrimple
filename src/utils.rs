use core::slice;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::ffi::OsStr;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::iter::FusedIterator;
use std::mem::transmute;
use std::ops::Deref;
use std::path::Path;
use std::ptr::copy_nonoverlapping;
use std::str::from_utf8_unchecked;

pub type Result<T = (), E = anyhow::Error> = std::result::Result<T, E>;

pub fn is_in<T: Eq>(items: impl AsRef<[T]>) -> impl Fn(&T) -> bool {
    move |item| items.as_ref().contains(item)
}

/// Exists to compare/print strings as if they had an additional char `P` before them, without
/// allocating
#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&P, f)?;
        Display::fmt(&self.0, f)
    }
}

pub fn default<T: Default>() -> T {
    T::default()
}

pub const fn os_str(x: &str) -> &OsStr {
    // SAFETY: `str` & `OsStr` have the same layout, and `OsStr`'s encoding is a superset of UTF-8
    unsafe { transmute(x) }
}

pub trait BoolExt {
    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E>;
    fn pick<T>(self, on_true: T, on_false: T) -> T;
}

impl BoolExt for bool {
    fn then_try<T, E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<Option<T>, E> {
        if self { Ok(Some(f()?)) } else { Ok(None) }
    }
    fn pick<T>(self, on_true: T, on_false: T) -> T {
        if self { on_true } else { on_false }
    }
}

pub trait OptionExt<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E>;
}

impl<T> OptionExt<T> for Option<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E> {
        self.map(f).transpose()
    }
}

pub unsafe fn assume_static<T>(x: &T) -> &'static T {
    // SAFETY: up to the caller
    unsafe { transmute(x) }
}

pub unsafe fn assume_static_mut<T>(x: &mut T) -> &'static mut T {
    // SAFETY: up to the caller
    unsafe { transmute(x) }
}

#[derive(Clone, Copy)]
pub struct InlineStr<const CAP: usize = 15> {
    // safety invariant: `self.len <= CAP`
    len: u8,
    buf: [u8; CAP],
}

impl<const CAP: usize> Debug for InlineStr<CAP> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl<const CAP: usize> Display for InlineStr<CAP> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl<const CAP: usize> PartialEq for InlineStr<CAP> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl<const CAP: usize> Eq for InlineStr<CAP> {}

impl<const CAP: usize> PartialOrd for InlineStr<CAP> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const CAP: usize> Ord for InlineStr<CAP> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl<const CAP: usize> AsRef<str> for InlineStr<CAP> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<const CAP: usize> Deref for InlineStr<CAP> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl<const CAP: usize> Default for InlineStr<CAP> {
    fn default() -> Self {
        Self { len: 0, buf: [0; CAP] }
    }
}

impl<const CAP: usize> Write for InlineStr<CAP> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let Ok(s_len) = u8::try_from(s.len()) else {
            return Err(fmt::Error);
        };
        if self.len() + s_len as usize > CAP {
            return Err(fmt::Error);
        }
        // SAFETY: the condition above asserts that `self` has at least `s_len` free bytes
        unsafe {
            let rest = self.buf.as_mut_ptr().add(self.len());
            copy_nonoverlapping(s.as_ptr(), rest, s_len as usize);
            self.len += s_len;
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! inline_str {
    ($str:expr) => {{
        const RES: InlineStr = match InlineStr::new($str) {
            Some(x) => x,
            None => panic!("constructing a short string"),
        };
        RES
    }};

    ($str:expr, len: $len:literal) => {{
        const RES: InlineStr<$len> = match InlineStr::<$len>::new($str) {
            Some(x) => x,
            None => panic!("constructing a short string"),
        };
        RES
    }};
}

impl<const CAP: usize> InlineStr<CAP> {
    /// returns `None` if `src`'s length is over `CAP`
    #[allow(clippy::cast_possible_truncation, reason = " no other sensible way in const fn ")]
    pub const fn new(src: &str) -> Option<Self> {
        let len = src.len();
        if CAP > u8::MAX as usize || len > CAP {
            return None;
        }
        let mut buf = [0; CAP];
        let mut i = 0;
        let mut ptr = src.as_ptr();
        while i < len {
            // SAFETY: `ptr` & `len` are all parts of `src` & the loop condition asserts that `ptr`
            // won't be advanced for more than `len` bytes forward
            unsafe {
                buf[i] = *ptr;
                ptr = ptr.add(1);
            }
            i += 1;
        }
        Some(Self { len: len as u8, buf })
    }

    pub const fn len(&self) -> usize {
        self.len as usize
    }

    pub const fn as_str(&self) -> &str {
        // SAFETY: `new` & `write_str` are the only functions that modify the string, and they only
        // write `&str`s to it.
        unsafe { from_utf8_unchecked(slice::from_raw_parts(self.buf.as_ptr(), self.len as usize)) }
    }
}

#[cfg(windows)]
pub fn soft_link(original: impl AsRef<Path>, link: impl AsRef<Path>) -> std::io::Result<()> {
    std::fs::hard_link(original, link)
}

#[cfg(unix)]
pub fn soft_link(original: impl AsRef<Path>, link: impl AsRef<Path>) -> std::io::Result<()> {
    std::os::unix::fs::symlink(original, link)
}

pub fn rel_link_to_file_path(link: &str) -> Cow<'_, Path> {
    let normalised: &Path = link
        .split_once('#')
        .map_or(link, |(path, _query)| path) // removing the fragment
        .trim_start_matches('/') // ensuring the path is relative
        .as_ref();
    if normalised.as_os_str().is_empty() {
        return Cow::Borrowed("index.html".as_ref());
    }
    match normalised.extension() {
        Some(_) => Cow::Borrowed(normalised),
        None => Cow::Owned(normalised.with_extension("html")),
    }
}

/// Copied from the std just to have [`Peekable::inner`] & [`Peekable::into_inner`] methods
// TODO: make a PR to add this to the std
pub struct Peekable<I: Iterator> {
    iter: I,
    /// Remember a peeked value, even if it was None.
    #[expect(clippy::option_option)]
    peeked: Option<Option<I::Item>>,
}

impl<I: Iterator> Peekable<I> {
    pub const fn new(iter: I) -> Self {
        Self { iter, peeked: None }
    }
}

// Peekable must remember if a None has been seen in the `.peek()` method.
// It ensures that `.peek(); .peek();` or `.peek(); .next();` only advances the
// underlying iterator at most once. This does not by itself make the iterator
// fused.
impl<I: Iterator> Iterator for Peekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.iter.next(),
        }
    }

    fn count(mut self) -> usize {
        match self.peeked.take() {
            Some(None) => 0,
            Some(Some(_)) => 1 + self.iter.count(),
            None => self.iter.count(),
        }
    }

    fn nth(&mut self, n: usize) -> Option<I::Item> {
        match self.peeked.take() {
            Some(None) => None,
            Some(v @ Some(_)) if n == 0 => v,
            Some(Some(_)) => self.iter.nth(n - 1),
            None => self.iter.nth(n),
        }
    }

    fn last(mut self) -> Option<I::Item> {
        let peek_opt = match self.peeked.take() {
            Some(None) => return None,
            Some(v) => v,
            None => None,
        };
        self.iter.last().or(peek_opt)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let peek_len = match self.peeked {
            Some(None) => return (0, Some(0)),
            Some(Some(_)) => 1,
            None => 0,
        };
        let (lo, hi) = self.iter.size_hint();
        let lo = lo.saturating_add(peek_len);
        let hi = match hi {
            Some(x) => x.checked_add(peek_len),
            None => None,
        };
        (lo, hi)
    }

    fn fold<Acc, Fold: FnMut(Acc, Self::Item) -> Acc>(self, init: Acc, mut fold: Fold) -> Acc {
        let acc = match self.peeked {
            Some(None) => return init,
            Some(Some(v)) => fold(init, v),
            None => init,
        };
        self.iter.fold(acc, fold)
    }
}

impl<I: DoubleEndedIterator> DoubleEndedIterator for Peekable<I> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.peeked.as_mut() {
            Some(v @ Some(_)) => self.iter.next_back().or_else(|| v.take()),
            Some(None) => None,
            None => self.iter.next_back(),
        }
    }

    fn rfold<Acc, Fold: FnMut(Acc, Self::Item) -> Acc>(self, init: Acc, mut fold: Fold) -> Acc {
        match self.peeked {
            Some(None) => init,
            Some(Some(v)) => {
                let acc = self.iter.rfold(init, &mut fold);
                fold(acc, v)
            }
            None => self.iter.rfold(init, fold),
        }
    }
}

impl<I: ExactSizeIterator> ExactSizeIterator for Peekable<I> {}

impl<I: FusedIterator> FusedIterator for Peekable<I> {}

impl<I: Iterator> Peekable<I> {
    /// The only reason I've copied this impl
    pub const fn inner(&self) -> &I {
        &self.iter
    }

    pub fn into_inner(self) -> I {
        self.iter
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).as_ref()
    }

    pub fn peek_mut(&mut self) -> Option<&mut I::Item> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).as_mut()
    }

    pub fn next_if(&mut self, func: impl FnOnce(&I::Item) -> bool) -> Option<I::Item> {
        match self.next() {
            Some(matched) if func(&matched) => Some(matched),
            other => {
                // Since we called `self.next()`, we consumed `self.peeked`.
                assert!(self.peeked.is_none());
                self.peeked = Some(other);
                None
            }
        }
    }

    pub fn next_if_eq<T: ?Sized>(&mut self, expected: &T) -> Option<I::Item>
    where
        I::Item: PartialEq<T>,
    {
        self.next_if(|next| next == expected)
    }

    pub fn next_if_map<R>(&mut self, f: impl FnOnce(I::Item) -> Result<R, I::Item>) -> Option<R> {
        let unpeek = if let Some(item) = self.next() {
            match f(item) {
                Ok(result) => return Some(result),
                Err(item) => Some(item),
            }
        } else {
            None
        };
        self.peeked = Some(unpeek);
        None
    }

    pub fn next_if_map_mut<R>(&mut self, f: impl FnOnce(&mut I::Item) -> Option<R>) -> Option<R> {
        let unpeek = if let Some(mut item) = self.next() {
            match f(&mut item) {
                Some(result) => return Some(result),
                None => Some(item),
            }
        } else {
            None
        };
        self.peeked = Some(unpeek);
        None
    }
}
