use {
    shrimple_parser::{pattern::Pattern, tuple::first, Input},
    std::{
        cmp::Ordering,
        ffi::OsStr,
        fmt::{Debug, Display, Formatter},
        hash::{Hash, Hasher},
        iter::FusedIterator,
        marker::PhantomData,
        mem::{replace, take, transmute},
        ops::{Deref, Not},
        path::{Path, PathBuf},
        ptr::NonNull,
        sync::Arc,
    }
};

pub type StrView = View<'static, str>;
pub type OsStrView = View<'static, OsStr>;
pub type PathView = View<'static, Path>;
pub type ByteView = View<'static, [u8]>;

/// Stores (a substring of) either a borrowed T or a T shared via an Arc
pub struct View<'borrow, T: ?Sized + 'borrow> {
    ptr: NonNull<T>,
    /// If this is [`usize::MAX`], this is a borrowed string, and no Drop logic is needed.
    /// At the very worst, this means leaking an Arc if it has devoured the entirety of the address
    /// space, which isn't the worst thing to happen in such a peculiar circumstance.
    off: usize,
    _covar: PhantomData<&'borrow str>,
}

// Safety: `View` is a compressed version of `Result<&T, Arc<T>>`
unsafe impl<'borrow, T: ?Sized + 'borrow> Send for View<'borrow, T>
where
    Result<&'borrow T, Arc<T>>: Send {}
// Safety: `View` is a compressed version of `Result<&T, Arc<T>>`
unsafe impl<'borrow, T: ?Sized + 'borrow> Sync for View<'borrow, T>
where
    Result<&'borrow T, Arc<T>>: Sync {}

impl<'borrow, T: ?Sized> From<&'borrow T> for View<'borrow, T> {
    fn from(value: &'borrow T) -> Self {
        Self { ptr: value.into(), off: usize::MAX, _covar: PhantomData }
    }
}

impl<T: ?Sized> From<Arc<T>> for View<'static, T> {
    fn from(value: Arc<T>) -> Self {
        // Safety: `Arc::into_raw` always returns a non-null pointer
        let ptr = unsafe {
            NonNull::new_unchecked(Arc::into_raw(value).cast_mut())
        };
        Self { ptr, off: 0, _covar: PhantomData }
    }
}

impl<T: ?Sized> From<Box<T>> for View<'static, T> {
    fn from(value: Box<T>) -> Self {
        Arc::<T>::from(value).into()
    }
}

impl<T> From<Vec<T>> for View<'static, [T]> {
    fn from(value: Vec<T>) -> Self {
        Arc::<[T]>::from(value).into()
    }
}

impl From<String> for View<'static, str> {
    fn from(value: String) -> Self {
        Arc::<str>::from(value).into()
    }
}

impl From<PathBuf> for View<'static, Path> {
    fn from(value: PathBuf) -> Self {
        Arc::<Path>::from(value).into()
    }
}

impl<T: ?Sized + 'static> Default for View<'_, T>
where
    &'static T: Default
{
    fn default() -> Self {
        <&T as Default>::default().into()
    }
}

impl<T: ?Sized> Drop for View<'_, T> {
    fn drop(&mut self) {
        if self.off != usize::MAX {
            // Safety: if `off` isn't `usize::MAX`, it's the offset from the start of the memory
            // spanned by the Arc
            unsafe {
                Arc::decrement_strong_count(self.ptr.byte_sub(self.off).as_ptr());
            }
        }
    }
}

impl<T: ?Sized> AsRef<T> for View<'_, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<T: ?Sized> Deref for View<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        // Safety: `self.ptr` always stores the pointer to the start of the view
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: ?Sized + Hash> Hash for View<'_, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<T: ?Sized + PartialEq, Other: Deref<Target = T>> PartialEq<Other> for View<'_, T> {
    fn eq(&self, other: &Other) -> bool {
        **self == **other
    }
}

impl<T: ?Sized + Eq> Eq for View<'_, T> {}

impl<T: ?Sized + PartialOrd, Other: Deref<Target = T>> PartialOrd<Other> for View<'_, T> {
    fn partial_cmp(&self, other: &Other) -> Option<Ordering> {
        self.deref().partial_cmp(&**other)
    }
}

impl<T: ?Sized + Ord> Ord for View<'_, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.deref().cmp(&**other)
    }
}

impl<T: ?Sized + Debug> Debug for View<'_, T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + Display> Display for View<'_, T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&**self, f)
    }
}

impl<'borrow, T: ?Sized> Clone for View<'borrow, T> {
    fn clone(&self) -> Self {
        if self.off != usize::MAX {
            // Safety: if `off` isn't `usize::MAX`, it's the offset from the start of the memory
            // spanned by the Arc
            unsafe {
                Arc::increment_strong_count(self.ptr.byte_sub(self.off).as_ptr());
            }
        }

        Self { ptr: self.ptr, off: self.off, _covar: PhantomData }
    }
}

impl Input for View<'_, str> {
    fn before(mut self, index: usize) -> Self {
        self.truncate(index);
        self
    }

    fn after(mut self, index: usize) -> Self {
        self.advance(index);
        self
    }

    fn split_at(self, mid: usize) -> (Self, Self) {
        (self.clone().before(mid), self.after(mid))
    }
}

impl<'borrow, T: ?Sized> View<'borrow, T> {
    fn unpack(self) -> Result<&'borrow T, Arc<T>> {
        // Safety: see the struct definition for the invariants of the fields
        unsafe {
            if self.off == usize::MAX {
                Ok(self.ptr.as_ref())
            } else {
                Err(Arc::from_raw(self.ptr.byte_sub(self.off).as_ptr()))
            }
        }
    }
}

impl<'borrow> View<'borrow, str> {
    /// Unlike the `From<&'borrow str>` impl, this functions puts no lifetimes contraints, and
    /// clones the string slice instead.
    pub fn from_str(src: &str) -> Self {
        Arc::<str>::from(src).into()
    }

    #[expect(clippy::transmute_undefined_repr, reason = "it's actually defined")]
    pub const fn into_os_str_view(self) -> View<'borrow, OsStr> {
        // Safety: `str` & `OsStr` have the same layout
        unsafe { transmute(self) }
    }

    #[expect(clippy::transmute_undefined_repr, reason = "it's actually defined")]
    pub const fn into_path_view(self) -> View<'borrow, Path> {
        // Safety: `str` & `Path` have the same layout
        unsafe { transmute(self) }
    }

    pub fn truncate(&mut self, new_len: usize) {
        let ptr = self.deref();
        self.ptr = ptr[..new_len].into();
    }

    #[expect(
        clippy::arithmetic_side_effects,
        reason = "ur gonna run out of memory before getting that to panic"
    )]
    pub fn advance(&mut self, by: usize) {
        let ptr = self.deref();
        self.ptr = ptr[by..].into();
        if self.off != usize::MAX {
            self.off += by;
        }
    }

    pub fn split_off(&mut self, at: usize) -> Self {
        let res = self.clone().after(at);
        self.truncate(at);
        res
    }

    #[expect(clippy::needless_pass_by_value, reason = "use `.by_ref()` to pass by ref")]
    pub fn strip_prefix(self, prefix: impl Pattern) -> Result<Self, Self> {
        prefix.immediate_match(self).map(first)
    }

    pub fn lines(self) -> impl FusedIterator<Item = Self> {
        struct Iter<'borrow> {
            rest: View<'borrow, str>,
        }

        impl<'borrow> Iterator for Iter<'borrow> {
            type Item = View<'borrow, str>;

            fn next(&mut self) -> Option<Self::Item> {
                let Some(at) = self.rest.find('\n') else {
                    return self.rest.is_empty().not().then_some(take(&mut self.rest));
                };
                let mut new_rest = self.rest.split_off(at);
                new_rest.advance(1);
                Some(replace(&mut self.rest, new_rest))
            }
        }

        impl FusedIterator for Iter<'_> {}

        Iter { rest: self }
    }
}

#[test]
fn shared_str_split_at() {
    assert_eq!(
        StrView::from(Arc::from("abcdef")).split_at(3),
        (View::from("abc"), View::from("def"))
    );
}
