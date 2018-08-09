# Unreleased

Released YYYY/MM/DD

## Added

* TODO (or remove section if none)

## Changed

* TODO (or remove section if none)

## Deprecated

* TODO (or remove section if none)

## Removed

* TODO (or remove section if none)

## Fixed

* TODO (or remove section if none)

## Security

* TODO (or remove section if none)

--------------------------------------------------------------------------------

# 0.2.11

Released 2018/08/09

## Fixed

* Fixed builds using `no-default-features = true` to not accidentally enable
  `no_std` mode, which requires nightly rust, and break builds on non-nightly
  channels. Enabling the `no_std` mode now requires disabling the `std` feature
  *and* enabling the `alloc` feature.

--------------------------------------------------------------------------------

# 0.2.10

Released 2018/08/08

## Added

* Added support for `no_std`! This currently requires nightly Rust's `alloc`
  feature to get access to `BTreeMap`. Enable `no_std` support by building
  without the on-by-default `std` feature. [#148][]

## Fixed

* Fixed formatting of some conversion operators. [#149][]
* Fixed parsing some tricky symbols with template argument packs that came out
  of boost. [#150][] [#152][]

[#148]: https://github.com/gimli-rs/cpp_demangle/pull/148
[#149]: https://github.com/gimli-rs/cpp_demangle/pull/149
[#150]: https://github.com/gimli-rs/cpp_demangle/pull/150
[#152]: https://github.com/gimli-rs/cpp_demangle/pull/152

--------------------------------------------------------------------------------

# 0.2.9

Released 2018/05/14

## Fixed

* Fixed a few issues with parentheticals.
* Should not force recompilation via build.rs for every compile anymore (bug
  introduced in 0.2.8 when trying to make the package that is distributed on
  crates.io smaller).

--------------------------------------------------------------------------------

# 0.2.8

Released 2018/05/11

Bug fixes, more `libiberty` tests passing, and we can now parse and demangle all
but one symbol from Firefox's `libxul`:

```
Total number of libxul symbols:                       274346
Number of libxul symbols parsed:                      274345 (100.00%)
Number of libxul symbols demangled:                   274345 (100.00%)
Number of libxul symbols demangled same as libiberty: 227259 (82.84%)
```

## Fixed

* AFL.rs fuzzing integration is fixed for the new AFL.rs releases.
* Fixed formatting of constructors and destructors.
* Fixed parsing of the `<function-param>` production.
* Fixed parsing of call expression productions.
* Parsing an operator's operands will only parse as many operands as the
  operator's arity, instead of as many as it can.

--------------------------------------------------------------------------------

# 0.2.7

Released 2017/11/27

Making lots of progress on symbols found in the wild! Here are our stats for
symbols from Firefox's `libxul`:

```
Total number of libxul symbols:                       274346
Number of libxul symbols parsed:                      274319 (99.99%)
Number of libxul symbols demangled:                   274319 (99.99%)
Number of libxul symbols demangled same as libiberty: 199928 (72.88%)
```

Additionally, the `libiberty` test threshold bumped up from 70 to 83 during this
release.

## Added

* Added support for GCC's "global constructors" and "global destructors"
  extensions.
* Added support for GCC's extensions to the `<special-name>` production:
  construction vtables, typeinfo functions, TLS initialization functions, TLS
  wrapper functions.
* Added support for the now-defunct `<local-source-name>` production. My
  understanding is that this is from an older version of the ABI standard. It
  isn't in the current version, but all the other demanglers support it, so we
  will too.

## Changed

* `cpp_demangle` is now part of the `gimli-rs` GitHub organization. The
  canonical repository is now https://github.com/gimli-rs/cpp_demangle
* Literals are now formatted how `libiberty` formats them. For example,
  `foo<true>()` rather than `foo<1>()`.
* Unary operators are now formatted with parentheses, matching `libiberty`.

## Fixed

* Nested array types and multi-dimensional arrays are now mangled correctly.
* Nested function types and their qualifiers are now mangled correctly.
* Arrays of function types and function types with array return and parameter
  types are now mangled correctly.
* The `new` operator is now correctly formatted as `operator new` rather than
  `operatornew`. Same for the `delete` operator.

--------------------------------------------------------------------------------

# 0.2.6

Released 2017/11/08

## Added

* Added support for vector types
* Added support for ABI tags
* Added support for `<unqualified-name> ::= <closure-type-name>` productions

## Fixed

* Fixed erroneous insertions into the substitutions table with prefixes and
  nested names
* Well known components were previously incorrectly not permitted to prefix
  template arguments
