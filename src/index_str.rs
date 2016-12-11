//! TODO FITZGEN

use std::ops::{Range, RangeFrom, RangeTo};

/// TODO FITZGEN
#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub struct IndexStr<'a> {
    idx: usize,
    string: &'a [u8],
}

#[allow(dead_code)]
impl<'a> IndexStr<'a> {
    /// TODO FITZGEN
    #[inline]
    pub fn new(string: &'a [u8]) -> IndexStr<'a> {
        IndexStr {
            idx: 0,
            string: string,
        }
    }

    /// Return the length of the string.
    pub fn len(&self) -> usize { self.string.len() }

    /// TODO FITZGEN
    pub fn index(&self) -> usize {
        self.idx
    }

    /// Split the string in two at the given index, resulting in the tuple where
    /// the first item has range `[0, idx)`, and the second has range `[idx,
    /// len)`.
    ///
    /// Panics if the index is out of bounds.
    #[inline]
    pub fn split_at(&self, idx: usize) -> (IndexStr<'a>, IndexStr<'a>) {
        (self.range_to(..idx), self.range_from(idx..))
    }

    /// The same as `split_at`, but returns a `Result` rather than panicking
    /// when the index is out of bounds.
    #[inline]
    pub fn try_split_at(&self,
                        idx: usize)
                        -> Option<(IndexStr<'a>, IndexStr<'a>)> {
        if idx > self.len() {
            None
        } else {
            Some(self.split_at(idx))
        }
    }
}

/// # Range Methods
///
/// Unfortunately, `std::ops::Index` *must* return a reference, so we can't
/// implement `Index<Range<usize>>` to return a new `IndexStr` the way we would
/// like to. Instead, we abandon fancy indexing operators and have these plain
/// old methods.
///
/// All of these methods panic on an out-of-bounds index.
#[allow(dead_code)]
impl<'a> IndexStr<'a> {
    /// Take the given `start..end` range of the underlying string and return a
    /// new `IndexStr`.
    #[inline]
    pub fn range(&self, idx: Range<usize>) -> IndexStr<'a> {
        IndexStr {
            idx: self.idx + idx.start,
            string: &self.string[idx],
        }
    }

    /// Take the given `start..` range of the underlying string and return a new
    /// `IndexStr`.
    #[inline]
    pub fn range_from(&self, idx: RangeFrom<usize>) -> IndexStr<'a> {
        IndexStr {
            idx: self.idx + idx.start,
            string: &self.string[idx],
        }
    }

    /// Take the given `..end` range of the underlying string and return a new
    /// `IndexStr`.
    #[inline]
    pub fn range_to(&self, idx: RangeTo<usize>) -> IndexStr<'a> {
        IndexStr {
            idx: self.idx,
            string: &self.string[idx],
        }
    }
}

impl<'a> AsRef<[u8]> for IndexStr<'a> {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.string
    }
}

impl<'a> From<&'a [u8]> for IndexStr<'a> {
    fn from(s: &[u8]) -> IndexStr { IndexStr::new(s) }
}

impl<'a, 'b> PartialEq<&'a [u8]> for IndexStr<'b> {
    fn eq(&self, rhs: &&[u8]) -> bool { self.string == *rhs }
}
