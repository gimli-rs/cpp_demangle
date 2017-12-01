//! This crate can parse a C++ “mangled” linker symbol name into a Rust value
//! describing what the name refers to: a variable, a function, a virtual table,
//! etc. The description type implements `Display`, producing human-readable
//! text describing the mangled name. Debuggers and profilers can use this crate
//! to provide more meaningful output.
//!
//! C++ requires the compiler to choose names for linker symbols consistently
//! across compilation units, so that two compilation units that have seen the
//! same declarations can pair up definitions in one unit with references in
//! another.  Almost all platforms other than Microsoft Windows follow the
//! [Itanium C++ ABI][itanium]'s rules for this.
//!
//! [itanium]: http://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle
//!
//! For example, suppose a C++ compilation unit has the definition:
//!
//! ```c++
//! namespace space {
//!   int foo(int x, int y) { return x+y; }
//! }
//! ```
//!
//! The Itanium C++ ABI specifies that the linker symbol for that function must
//! be named `_ZN5space3fooEii`. This crate can parse that name into a Rust
//! value representing its structure. Formatting the value with the `format!`
//! macro or the `std::string::ToString::to_string` trait method yields the
//! string `space::foo(int, int)`, which is more meaningful to the C++
//! developer.

#![deny(missing_docs)]
#![deny(missing_debug_implementations)]
#![deny(unsafe_code)]
// Clippy stuff.
#![allow(unknown_lints)]
#![allow(inline_always)]

#[macro_use]
mod logging;

pub mod ast;
pub mod error;
mod index_str;
mod subs;

use ast::{Demangle, Parse, ParseContext};
use error::{Error, Result};
use index_str::IndexStr;
use std::fmt;
use std::io;

/// Options to control the demangling process.
#[derive(Clone, Copy, Debug, Default)]
pub struct DemangleOptions {
    /// Do not display function arguments.
    pub no_params: bool,
}

/// A `Symbol` which owns the underlying storage for the mangled name.
pub type OwnedSymbol = Symbol<Vec<u8>>;

/// A `Symbol` which borrows the underlying storage for the mangled name.
pub type BorrowedSymbol<'a> = Symbol<&'a [u8]>;

/// A mangled symbol that has been parsed into an AST.
///
/// This is generic over some storage type `T` which can be either owned or
/// borrowed. See the `OwnedSymbol` and `BorrowedSymbol` type aliases.
#[derive(Clone, Debug, PartialEq)]
pub struct Symbol<T> {
    raw: T,
    substitutions: subs::SubstitutionTable,
    parsed: ast::MangledName,
}

impl<T> Symbol<T>
where
    T: AsRef<[u8]>,
{
    /// Given some raw storage, parse the mangled symbol from it.
    ///
    /// ```
    /// use cpp_demangle::Symbol;
    /// use std::string::ToString;
    ///
    /// // First, something easy :)
    ///
    /// let mangled = b"_ZN5space3fooEibc";
    ///
    /// let sym = Symbol::new(&mangled[..])
    ///     .expect("Could not parse mangled symbol!");
    ///
    /// let demangled = sym.to_string();
    /// assert_eq!(demangled, "space::foo(int, bool, char)");
    ///
    /// // Now let's try something a little more complicated!
    ///
    /// let mangled =
    ///     b"__Z28JS_GetPropertyDescriptorByIdP9JSContextN2JS6HandleIP8JSObjectEENS2_I4jsidEENS1_13MutableHandleINS1_18PropertyDescriptorEEE";
    ///
    /// let sym = Symbol::new(&mangled[..])
    ///     .expect("Could not parse mangled symbol!");
    ///
    /// let demangled = sym.to_string();
    /// assert_eq!(
    ///     demangled,
    ///     "JS_GetPropertyDescriptorById(JSContext*, JS::Handle<JSObject*>, JS::Handle<jsid>, JS::MutableHandle<JS::PropertyDescriptor>)"
    /// );
    /// ```
    pub fn new(raw: T) -> Result<Symbol<T>> {
        let mut substitutions = subs::SubstitutionTable::new();

        let parsed = {
            let ctx = ParseContext::default();
            let input = IndexStr::new(raw.as_ref());

            let (parsed, tail) = ast::MangledName::parse(&ctx, &mut substitutions, input)?;
            debug_assert!(ctx.recursion_level() == 0);

            if tail.is_empty() {
                parsed
            } else {
                return Err(Error::UnexpectedText.into());
            }
        };

        let symbol = Symbol {
            raw: raw,
            substitutions: substitutions,
            parsed: parsed,
        };

        if cfg!(feature = "logging") {
            println!(
                "Successfully parsed '{}' as

AST = {:#?}

substitutions = {:#?}",
                String::from_utf8_lossy(symbol.raw.as_ref()),
                symbol.parsed,
                symbol.substitutions
            );
        }

        Ok(symbol)
    }

    /// Demangle the symbol and return it as a String.
    ///
    /// Unlike the `ToString` implementation, this function allows options to
    /// be specified.
    ///
    /// ```
    /// use cpp_demangle::{DemangleOptions, Symbol};
    /// use std::string::ToString;
    ///
    /// let mangled = b"_ZN5space3fooEibc";
    ///
    /// let sym = Symbol::new(&mangled[..])
    ///     .expect("Could not parse mangled symbol!");
    ///
    /// let demangled = sym.to_string();
    /// let options = DemangleOptions::default();
    /// let demangled_again = sym.demangle(&options).unwrap();
    /// assert_eq!(demangled_again, demangled);
    /// ```
    pub fn demangle(&self, options: &DemangleOptions) -> io::Result<String> {
        let mut out = vec![];
        {
            let mut ctx = ast::DemangleContext::new(
                &self.substitutions,
                self.raw.as_ref(),
                options,
                &mut out,
            );
            self.parsed.demangle(&mut ctx, None)?;
        }

        Ok(String::from_utf8(out).unwrap())
    }
}

impl<T> Symbol<T> {
    /// Parse a mangled symbol from input and return it and the trailing tail of
    /// bytes that come after the symbol.
    ///
    /// While `Symbol::new` will return an error if there is unexpected trailing
    /// bytes, `with_tail` simply returns the trailing bytes along with the
    /// parsed symbol.
    ///
    /// ```
    /// use cpp_demangle::BorrowedSymbol;
    /// use std::string::ToString;
    ///
    /// let mangled = b"_ZN5space3fooEibc and some trailing junk";
    ///
    /// let (sym, tail) = BorrowedSymbol::with_tail(&mangled[..])
    ///     .expect("Could not parse mangled symbol!");
    ///
    /// assert_eq!(tail, b" and some trailing junk");
    ///
    /// let demangled = sym.to_string();
    /// assert_eq!(demangled, "space::foo(int, bool, char)");
    /// ```
    pub fn with_tail(input: &[u8]) -> Result<(BorrowedSymbol, &[u8])> {
        let mut substitutions = subs::SubstitutionTable::new();

        let ctx = ParseContext::default();
        let idx_str = IndexStr::new(input);
        let (parsed, tail) = ast::MangledName::parse(&ctx, &mut substitutions, idx_str)?;
        debug_assert!(ctx.recursion_level() == 0);

        let symbol = Symbol {
            raw: input,
            substitutions: substitutions,
            parsed: parsed,
        };

        if cfg!(feature = "logging") {
            println!(
                "Successfully parsed '{}' as

AST = {:#?}

substitutions = {:#?}",
                String::from_utf8_lossy(symbol.raw.as_ref()),
                symbol.parsed,
                symbol.substitutions
            );
        }

        Ok((symbol, tail.into()))
    }
}

impl<T> fmt::Display for Symbol<T>
where
    T: AsRef<[u8]>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = vec![];
        {
            let options = DemangleOptions::default();
            let mut ctx = ast::DemangleContext::new(
                &self.substitutions,
                self.raw.as_ref(),
                &options,
                &mut out,
            );
            self.parsed.demangle(&mut ctx, None).map_err(|err| {
                log!("Demangling error: {:#?}", err);
                fmt::Error
            })?;
        }
        write!(f, "{}", String::from_utf8_lossy(&out))
    }
}
