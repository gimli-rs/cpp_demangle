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
//! [itanium]: http://mentorembedded.github.io/cxx-abi/abi.html#mangling
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
extern crate rental;

#[macro_use]
mod logging;

pub mod ast;
pub mod error;
mod index_str;
mod subs;

use ast::{Demangle, Parse};
use error::{Error, Result};
use index_str::IndexStr;
use std::fmt;

#[allow(missing_docs)]
mod symbol_data {
    use std::fmt;

    // XXX: TODO FITZGEN

    rental! {
        #[allow(unsafe_code)]
        #[allow(missing_debug_implementations)]
        mod symbol_data {
            use ast;
            use subs;

            #[rental]
            pub struct SymbolData {
                parsed: Box<(subs::SubstitutionTable, ast::MangledName)>,
                resolution: ast::Resolution<'parsed>,
            }
        }
    }

    pub use self::symbol_data::*;

    impl fmt::Debug for SymbolData {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "SymbolData {{ ... }}")
        }
    }

    // TODO FITZGEN: implement helper methods for accessing the subs and mangled
    // name

    pub fn new(subs: super::subs::SubstitutionTable,
               ast: super::ast::MangledName)
               -> super::error::Result<SymbolData> {

        fn new_resolution<'a>(&(ref subs, ref ast): &'a (super::subs::SubstitutionTable,
                                                         super::ast::MangledName))
                              -> super::error::Result<super::ast::Resolution<'a>> {
            super::ast::Resolution::new(subs, ast)
        }

        SymbolData::try_new(Box::new((subs, ast)), new_resolution)
            .map_err(|e| e.0)
    }

}

/// TODO FITZGEN TEMPORARY
pub use symbol_data::*;

/// A `Symbol` which owns the underlying storage for the mangled name.
pub type OwnedSymbol = Symbol<Vec<u8>>;

/// A `Symbol` which borrows the underlying storage for the mangled name.
pub type BorrowedSymbol<'a> = Symbol<&'a [u8]>;

/// A mangled symbol that has been parsed into an AST.
///
/// This is generic over some storage type `T` which can be either owned or
/// borrowed. See the `OwnedSymbol` and `BorrowedSymbol` type aliases.
#[derive(Debug)]
pub struct Symbol<T> {
    raw: T,
    data: symbol_data::SymbolData,
}

impl<T> Symbol<T>
    where T: AsRef<[u8]>
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
            let input = IndexStr::new(raw.as_ref());
            let (parsed, tail) = try!(ast::MangledName::parse(&mut substitutions, input));
            if tail.is_empty() {
                parsed
            } else {
                return Err(Error::UnexpectedText.into());
            }
        };

        let data = try!(symbol_data::new(substitutions, parsed));

        let symbol = Symbol {
            raw: raw,
            data: data,
        };

        if cfg!(feature = "logging") {
            symbol.data.rent_all(|data| {
                println!("Successfully parsed '{}' as

AST = {:#?}

substitutions = {:#?}",
                         String::from_utf8_lossy(symbol.raw.as_ref()),
                         data.parsed.1,
                         data.parsed.0);
            });
        }

        Ok(symbol)
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

        let idx_str = IndexStr::new(input);
        let (parsed, tail) = try!(ast::MangledName::parse(&mut substitutions, idx_str));
        let data = try!(symbol_data::new(substitutions, parsed));

        let symbol = Symbol {
            raw: input,
            data: data,
        };

        if cfg!(feature = "logging") {
            symbol.data.rent_all(|data| {
                println!("Successfully parsed '{}' as

AST = {:#?}

substitutions = {:#?}",
                         String::from_utf8_lossy(symbol.raw.as_ref()),
                         data.parsed.1,
                         data.parsed.0);
            });
        }

        Ok((symbol, tail.into()))
    }
}

impl<T> fmt::Display for Symbol<T>
    where T: AsRef<[u8]>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = vec![];

        try!(self.data.rent_all(|data| {
            let mut ctx = ast::DemangleContext::new(&data.parsed.0,
                                                    self.raw.as_ref(),
                                                    &mut out);
            data.parsed.1.demangle(&mut ctx, None).map_err(|_| fmt::Error)
        }));

        write!(f, "{}", String::from_utf8_lossy(&out))
    }
}
