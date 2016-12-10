//! TODO FITZGEN

use std::ops::{Range, RangeFrom, RangeTo};

/// TODO FITZGEN
pub struct IndexStr<'a> {
    idx: usize,
    string: &'a str,
}

#[allow(dead_code)]
impl<'a> IndexStr<'a> {
    /// TODO FITZGEN
    #[inline]
    pub fn new(string: &'a str) -> IndexStr<'a> {
        IndexStr {
            idx: 0,
            string: string,
        }
    }

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
        if idx > self.string.len() {
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

impl<'a> AsRef<str> for IndexStr<'a> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.string
    }
}

