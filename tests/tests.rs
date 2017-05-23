#![allow(non_snake_case)]

extern crate cpp_demangle;
extern crate diff;

use std::io::Write;

fn assert_demangles_as(mangled: &str, expected: &str) {
    let sym = cpp_demangle::BorrowedSymbol::new(mangled.as_bytes())
        .expect("should parse mangled symbol ok");

    let mut actual = vec![];
    write!(&mut actual, "{}", sym).expect("should demangle symbol ok");
    let actual = String::from_utf8(actual).expect("should demangle to valid utf-8");

    if expected != actual {
        println!("");
        println!("Diff:");
        println!("--- expected");
        print!("+++ actual");

        let mut last = None;
        for cmp in diff::chars(expected, &actual) {
            match (last, cmp.clone()) {
                (Some(diff::Result::Left(_)), diff::Result::Left(_)) |
                (Some(diff::Result::Both(..)), diff::Result::Both(..)) |
                (Some(diff::Result::Right(_)), diff::Result::Right(_)) => {}

                (_, diff::Result::Left(_))  => print!("\n-"),
                (_, diff::Result::Both(..))  => print!("\n "),
                (_, diff::Result::Right(_)) => print!("\n+"),
            };
            match cmp.clone() {
                diff::Result::Left(c) |
                diff::Result::Both(c, _) |
                diff::Result::Right(c) => print!("{}", c),
            }
            last = Some(cmp);
        }
        println!("");
    }

    assert_eq!(expected, actual);
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
demangles!(_Z3fooISt6vectorIiEEvv,
           "void foo<std::vector<int> >()");

// Test cases found via differential testing against `c++filt` with `cargo-fuzz`
// and `libFuzzer`.

demangles!(_Z5ccc_Z5cccmmmml,
           "ccc_Z(cccmm, unsigned long, unsigned long, long)");
