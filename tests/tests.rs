#![allow(non_snake_case)]

extern crate cpp_demangle;

use std::io::Write;

fn assert_demangles_as(mangled: &str, demangled: &str) {
    let sym = cpp_demangle::BorrowedSymbol::new(mangled.as_bytes())
        .expect("should parse mangled symbol ok");

    let mut actual = vec![];
    write!(&mut actual, "{}", sym).expect("should demangle symbol ok");
    let actual = String::from_utf8(actual).expect("should demangle to valid utf-8");

    assert_eq!(demangled, actual);
}

macro_rules! demangles {
    ( $mangled:ident , $demangled:expr ) => {
        #[test]
        fn $mangled() {
            assert_demangles_as(stringify!($mangled), $demangled);
        }
    }
}

demangles!(_Z20instantiate_with_intI3FooET_IiEv,
           "Foo<int> instantiate_with_int<Foo>()");
