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

extern crate fixedbitset;
extern crate stable_deref_trait;

#[macro_use]
mod logging;

pub mod ast;
pub mod error;
mod index_str;
mod res;
mod subs;

use ast::{Demangle, Parse};
use error::{Error, Result};
use index_str::IndexStr;
use std::fmt;

#[allow(unsafe_code)]
mod encapsulate_unsafety {
    //! A module encapsulating, hiding, and enforcing the privacy of
    //! `SymbolData`'s underlying unsafe code.
    //!
    //! # Why is there unsafe code?
    //!
    //! The `SymbolData` struct has members which borrow from each other. This
    //! is something which Rust's borrow checker cannot verify is safe, and
    //! therefore requires `unsafe` code blocks, and manually auditing them for
    //! safety and correctness.
    //!
    //! # Why encapsulate the unsafe code in a module?
    //!
    //! To limit the amount of code that must be manually audited to verify
    //! `SymbolData`'s safety, we encapsulate `SymbolData` in its own module. A
    //! struct's non-public fields cannot be accessed by code outside of the
    //! module it is defined it, so by putting `SymbolData` in its own module,
    //! we only need to audit the public APIs exposed from this module for
    //! safety.
    //!
    //! # Why is `SymbolData` safe?
    //!
    //! The `SymbolData` struct is made of three parts:
    //!
    //! 1. The substitutions table as a `subs::SubstitutionTable`.
    //! 2. The parsed AST as a `Box<ast::MangledName>`.
    //! 3. The template scope resolution, `res::Resolution`, which borrows from
    //!    (1) and (2).
    //!
    //! In the general case, borrows are invalidated by the borrowee moving, so
    //! this would not be safe if the `SymbolData` ever moved. However, if we
    //! heap allocate (and never re-allocate afterwards ) the data being
    //! borrowed, it will have a stable address even if the stack allocated
    //! handle to the heap allocation moves. This concept of dereferencing to a
    //! stable address is represented by the `StableDeref` marker trait. It is
    //! an unsafe trait, so we its implementations should be audited for
    //! correctness. In our case that is `Box<T>` and
    //! `subs::SubstitutionTable`.
    //!
    //! `Box<T>` is just an owning heap pointer. It does no reallocation, and
    //! therefore as long as we do not mutate its boxed value after we borrow
    //! from it, any borrowed references are stable regardless if the `Box<T>`
    //! on the stack is moved.
    //!
    //! The `subs::SubstitutionTable` is a newtype over a `Vec`, which heap
    //! allocates its elements. Therefore, as long as we don't modify the
    //! underlying `Vec`'s elements or resize them by pushing or popping
    //! elements, references we get out of it are stable.
    //!
    //! **Invariant:** A `SymbolData`'s `subs::SubstitutionTable` and
    //! `Box<ast::MangledName>` are `StableDeref` and are never mutated once
    //! ownership is given to the `SymbolData`.
    //!
    //! Finally, we have to make sure that the borrows don't outlast the
    //! borrowed values. We `std::mem::transmute` the `res::Resolution`'s
    //! lifetime parameter to `'static`, so it is up to us to manually uphold
    //! this property. Rust does not guarantee a struct's members' drop order,
    //! so we provide a custom `Drop` implementation for `SymbolData` that wipes
    //! its resolution, removing the resolution's borrows before the members
    //! being borrowed from are dropped.
    //!
    //! **Invariant:** A `SymbolData`'s `resolution` is (effectively) dropped
    //! before its other members.

    use ast;
    use error::Result;
    use res;
    use stable_deref_trait::StableDeref;
    use std::mem;
    use subs;

    /// Dereference something to a stable address. It is the caller's
    /// responsibility to ensure that the resulting reference does not outlive
    /// the referenced data.
    unsafe fn stable_deref_as_static<T, U>(it: &T) -> &'static U
        where T: StableDeref<Target = U>,
              U: ?Sized
    {
        mem::transmute(it.deref())
    }

    /// The internal data that makes up a parsed C++ symbol.
    #[derive(Debug)]
    pub struct SymbolData {
        substitutions: subs::SubstitutionTable,
        parsed: Box<ast::MangledName>,
        resolution: res::Resolution<'static>,
    }

    impl Drop for SymbolData {
        fn drop(&mut self) {
            // Ensure that the resolution's references into the other
            // `SymbolData` members are dropped first.
            self.resolution.clear();
        }
    }

    impl SymbolData {
        /// Construct a new `SymbolData`.
        pub fn new(substitutions: subs::SubstitutionTable,
                   parsed: ast::MangledName)
                   -> Result<SymbolData> {
            let parsed = Box::new(parsed);

            let resolution = try!(unsafe {
                res::Resolution::new(stable_deref_as_static(&substitutions),
                                     stable_deref_as_static(&parsed))
            });

            Ok(SymbolData {
                substitutions: substitutions,
                parsed: parsed,
                resolution: resolution,
            })
        }

        /// Get this symbol's substitutions table.
        pub fn substitutions(&self) -> &subs::SubstitutionTable {
            &self.substitutions
        }

        /// Get the root of this symbol's AST.
        pub fn ast(&self) -> &ast::MangledName {
            &*self.parsed
        }

        /// Get the scope resolution for this symbol's AST.
        pub fn resolution(&self) -> &res::Resolution {
            &self.resolution
        }
    }
}

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
    data: encapsulate_unsafety::SymbolData,
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

        if cfg!(feature = "logging") {
            println!("Successfully parsed '{}' as

AST = {:#?}

substitutions = {:#?}",
                     String::from_utf8_lossy(raw.as_ref()),
                     parsed,
                     substitutions);
        }

        let data = try!(encapsulate_unsafety::SymbolData::new(substitutions, parsed));

        let symbol = Symbol {
            raw: raw,
            data: data,
        };

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

        if cfg!(feature = "logging") {
            println!("Successfully parsed '{}' as

AST = {:#?}

substitutions = {:#?}",
                     String::from_utf8_lossy(input),
                     parsed,
                     substitutions);
        }

        let data = try!(encapsulate_unsafety::SymbolData::new(substitutions, parsed));

        let symbol = Symbol {
            raw: input,
            data: data,
        };

        Ok((symbol, tail.into()))
    }
}

impl<T> fmt::Display for Symbol<T>
    where T: AsRef<[u8]>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = vec![];

        {
            let mut ctx = ast::DemangleContext::new(&self.data.substitutions(),
                                                    self.data.resolution(),
                                                    self.raw.as_ref(),
                                                    &mut out);
            try!(self.data.ast().demangle(&mut ctx).map_err(|_| fmt::Error));
        }

        write!(f, "{}", String::from_utf8_lossy(&out))
    }
}
