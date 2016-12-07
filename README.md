# VAPORWARE `cpp-demangle`: a C++ linker symbol demangler

This library parses “mangled” linker symbol names used by C++ into a Rust value
describing the entity the name refers to: a variable, a function, a virtual
table, etc. This library also provides formatters for these descriptions,
suitable for developers to read.

C++ requires the compiler to choose names for linker symbols consistently across
compilation units, so that two compilation units that have seen the same
declarations can pair up definitions in one unit with references in another.
Almost all platforms other than Microsoft Windows follow the
[Itanium C++ ABI][itanium]'s rules for this.

[itanium]: http://mentorembedded.github.io/cxx-abi/abi.html#mangling

