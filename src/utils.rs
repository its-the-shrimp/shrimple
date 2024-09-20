use core::slice;
use std::cmp::Ordering;
use std::ffi::OsStr;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::mem::transmute;
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

pub trait OptionExt<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E>;
}

impl<T> OptionExt<T> for Option<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E> {
        self.map(f).transpose()
    }
}

pub unsafe fn assume_static<T>(x: &T) -> &'static T {
    transmute(x)
}

pub unsafe fn assume_static_mut<T>(x: &mut T) -> &'static mut T {
    transmute(x)
}

#[derive(Clone, Copy)]
pub struct ShortStr<const CAP: usize = 15> {
    // safety invariant: `self.len <= CAP`
    len: u8,
    buf: [u8; CAP],
}

impl<const CAP: usize> Debug for ShortStr<CAP> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl<const CAP: usize> Display for ShortStr<CAP> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl<const CAP: usize> PartialEq for ShortStr<CAP> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl<const CAP: usize> Eq for ShortStr<CAP> {}

impl<const CAP: usize> PartialOrd for ShortStr<CAP> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const CAP: usize> Ord for ShortStr<CAP> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl<const CAP: usize> AsRef<str> for ShortStr<CAP> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<const CAP: usize> Default for ShortStr<CAP> {
    fn default() -> Self {
        Self { len: 0, buf: [0; CAP] }
    }
}

impl<const CAP: usize> Write for ShortStr<CAP> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let s_len: u8 = s.len().try_into().map_err(|_| fmt::Error)?;
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
macro_rules! short_str {
    ($str:expr) => {{
        const RES: ShortStr = match ShortStr::new($str) {
            Some(x) => x,
            None => panic!("constructing a short string"),
        };
        RES
    }};

    ($str:expr, len: $len:literal) => {{
        const RES: ShortStr<$len> = match ShortStr::<$len>::new($str) {
            Some(x) => x,
            None => panic!("constructing a short string"),
        };
        RES
    }};
}

impl<const CAP: usize> ShortStr<CAP> {
    /// returns `None` if `src`'s length is over `CAP`
    #[allow(clippy::cast_possible_truncation, /*reason=" no other sensible way in const fn "*/)]
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

pub trait StrExt {
    fn trim_fragment(&self) -> &Self;
}

impl StrExt for str {
    fn trim_fragment(&self) -> &Self {
        self.split_once('#').map_or(self, |(path, _frag)| path)
    }
}
