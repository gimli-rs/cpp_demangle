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

    /// Found a reference to a template arg that is either out-of-bounds, or in
    /// a context without template args.
    BadTemplateArgReference,

    /// Found a reference to a function arg that is either out-of-bounds, or in
    /// a context without function args.
    BadFunctionArgReference,

    /// An overflow or underflow would occur when parsing an integer in a
    /// mangled symbol.
    Overflow,

    /// The act of demangling some part of the AST attempted to demangle itself
    /// again.
    RecursiveDemangling,
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
            Error::BadTemplateArgReference => {
                write!(f, "reference to a template arg that is either out-of-bounds, or in a context without template args")
            }
            Error::BadFunctionArgReference => {
                write!(f, "reference to a function arg that is either out-of-bounds, or in a context without function args")
            }
            Error::Overflow => {
                write!(f,
                       "an overflow or underflow would occur when parsing an integer in a mangled symbol")
            }
            Error::RecursiveDemangling => {
                write!(f, "demangling some part of the AST attempted to demangle itself again")
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
            Error::BadTemplateArgReference => "reference to a template arg that is either out-of-bounds, or in a context without template args",
            Error::BadFunctionArgReference => "reference to a function arg that is either out-of-bounds, or in a context without function args",
            Error::Overflow => "an overflow or underflow would occur when parsing an integer in a mangled symbol",
            Error::RecursiveDemangling => "demangling some part of the AST attempted to demangle itself again",
        }
    }
}

/// A demangling result of `T` or a `cpp_demangle::error::Error`.
pub type Result<T> = ::std::result::Result<T, Error>;
