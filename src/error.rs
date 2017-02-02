//! Custom `Error` and `Result` types for the `cpp_demangle` crate.

use std::error;
use std::fmt;

/// Errors that can occur while demangling a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Error {
    /// The mangled symbol ends abruptly.
    UnexpectedEnd,

    /// The mangled symbol is not well-formed.
    UnexpectedText,

    /// Found a back reference that is out-of-bounds of the substitution
    /// table.
    BadBackReference,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::UnexpectedEnd => write!(f, "mangled symbol ends abruptly"),
            Error::UnexpectedText => write!(f, "mangled symbol is not well-formed"),
            Error::BadBackReference => {
                write!(f,
                       "back reference that is out-of-bounds of the substitution table")
            }
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::UnexpectedEnd => "mangled symbol ends abruptly",
            Error::UnexpectedText => "mangled symbol is not well-formed",
            Error::BadBackReference => "back reference that is out-of-bounds of the substitution table",
        }
    }
}

/// A demangling result of `T` or a `cpp_demangle::error::Error`.
pub type Result<T> = ::std::result::Result<T, Error>;
