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

                (_, diff::Result::Left(_)) => print!("\n-"),
                (_, diff::Result::Both(..)) => print!("\n "),
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

fn assert_does_not_demangle(s: &str) {
    if let Ok(sym) = cpp_demangle::BorrowedSymbol::new(s.as_bytes()) {
        panic!("Unexpectedly demangled '{}' as '{}'", s, sym);
    }
}

macro_rules! demangles {
    ( $mangled:ident , $demangled:expr ) => {
        demangles!($mangled, stringify!($mangled), $demangled);
    };
    ( $name:ident , $mangled:expr , $demangled:expr ) => {
        #[test]
        fn $name() {
            assert_demangles_as($mangled, $demangled);
        }
    }
}

macro_rules! does_not_demangle {
    ( $name:ident , $s:expr ) => {
        #[test]
        fn $name() {
            assert_does_not_demangle($s);
        }
    }
}

// This should definitely not parse and demangle as
// `operator()(unsigned __int128, short, long double)`.
does_not_demangle!(close_should_not_demangle, "close");

demangles!(
    _Z20instantiate_with_intI3FooET_IiEv,
    "Foo<int> instantiate_with_int<Foo>()"
);
demangles!(_Z3fooISt6vectorIiEEvv, "void foo<std::vector<int> >()");
demangles!(__ZN3foo3barE3quxS0_, "foo::bar(qux, qux)");
demangles!(__ZN3foo3barE3quxS_, "foo::bar(qux, foo)");

demangles!(
    _ZN4funcI2TyEEN6ResultIT_EES3_,
    "Result<Ty> func<Ty>(Result<Ty>)"
);
demangles!(_ZN4funcI2TyEEN6ResultIT_EES2_, "Result<Ty> func<Ty>(Ty)");
demangles!(
    _ZN4funcI2TyEEN6ResultIT_EES1_,
    "Result<Ty> func<Ty>(Result)"
);
demangles!(_ZN4funcI2TyEEN6ResultIT_EES0_, "Result<Ty> func<Ty>(Ty)");
demangles!(_ZN4funcI2TyEEN6ResultIT_EES_, "Result<Ty> func<Ty>(func)");

demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), Ty)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES0_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), Ty::method)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES1_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), Ty)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES2_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), char const)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES3_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), char const*)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES4_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), void (char const*))"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES5_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), void (Ty::*)(char const*))"
);

demangles!(_ZNK1fB5cxx11Ev, "f[abi:cxx11]() const");

demangles!(
    _ZN4base8internal14CheckedSubImplIlEENSt9enable_ifIXsrSt14numeric_limitsIT_E10is_integerEbE4typeES4_S4_PS4_,
    "std::enable_if<std::numeric_limits<long>::is_integer, bool>::type base::internal::CheckedSubImpl<long>(long, long, long*)"
);

demangles!(
    _ZZN7mozilla12EMEDecryptor5FlushEvENUlvE_D4Ev,
    "mozilla::EMEDecryptor::Flush()::{lambda()#0}::maybe in-charge destructor()"
);

demangles!(
    _ZSt4copyIPKcPcET0_T_S4_S3_,
    "char* std::copy<char const*, char*>(char const*, char const*, char*)"
);

demangles!(
    _Z9_mm_or_psDv4_fS_,
    "_mm_or_ps(float __vector(4), float __vector(4))"
);

demangles!(
    _ZN5space20templated_trampolineIPFvvEEEvT_,
    "void space::templated_trampoline<void (*)()>(void (*)())"
);

demangles!(
    _Z18convertCase_helperIN14QUnicodeTables14CasefoldTraitsEtET0_S2_,
    "unsigned short convertCase_helper<QUnicodeTables::CasefoldTraits, unsigned short>(unsigned short)"
);

demangles!(
    _ZnwmRKSt9nothrow_t,
    "operator new(unsigned long, std::nothrow_t const&)"
);

demangles!(
    _ZGRL13MozLangGroups_,
    "reference temporary #0 for MozLangGroups"
);

demangles!(_ZN11InstrumentsL8gSessionE, "Instruments::gSession");
demangles!(_ZTWN2js10TlsContextE, "TLS wrapper function for js::TlsContext");

demangles!(_Z3fooILb0EEvi, "void foo<false>(int)");
demangles!(_Z3fooILb1EEvi, "void foo<true>(int)");
demangles!(_Z3fooILb2EEvi, "void foo<(bool)2>(int)");
demangles!(_Z3fooILb999999EEvi, "void foo<(bool)999999>(int)");
demangles!(_Z3fooILbaaaaaaEEvi, "void foo<(bool)aaaaaa>(int)");
demangles!(bool_literal_with_decimal, "_Z3fooILb999.999EEvi", "void foo<(bool)999.999>(int)");
demangles!(_Z3fooILbn1EEvi, "void foo<(bool)-1>(int)");
demangles!(_Z3fooILbn0EEvi, "void foo<(bool)-0>(int)");

demangles!(_Z3fooILc65EEvi, "void foo<(char)65>(int)");
demangles!(_Z3fooILc48EEvi, "void foo<(char)48>(int)");
demangles!(_Z3fooILc0EEvi, "void foo<(char)0>(int)");
demangles!(_Z3fooILc999999EEvi, "void foo<(char)999999>(int)");
demangles!(_Z3fooILcaaaaaaEEvi, "void foo<(char)aaaaaa>(int)");
demangles!(char_literal_with_decimal, "_Z3fooILc999.999EEvi", "void foo<(char)999.999>(int)");
demangles!(_Z3fooILcn65EEvi, "void foo<(char)-65>(int)");
demangles!(char_literal_with_negative_sign, "_Z3fooILc-65EEvi", "void foo<(char)-65>(int)");

demangles!(_Z3fooILd65EEvi, "void foo<(double)[65]>(int)");
demangles!(_Z3fooILd48EEvi, "void foo<(double)[48]>(int)");
demangles!(_Z3fooILd0EEvi, "void foo<(double)[0]>(int)");
demangles!(_Z3fooILd999999EEvi, "void foo<(double)[999999]>(int)");
demangles!(_Z3fooILdaaaaaaEEvi, "void foo<(double)[aaaaaa]>(int)");
demangles!(double_literal_with_decimal, "_Z3fooILd999.999EEvi", "void foo<(double)[999.999]>(int)");
demangles!(_Z3fooILdn65EEvi, "void foo<(double)-[65]>(int)");
demangles!(double_literal_with_negative_sign, "_Z3fooILd-65EEvi", "void foo<(double)[-65]>(int)");

demangles!(_Z3fooILf65EEvi, "void foo<(float)[65]>(int)");
demangles!(_Z3fooILf48EEvi, "void foo<(float)[48]>(int)");
demangles!(_Z3fooILf0EEvi, "void foo<(float)[0]>(int)");
demangles!(_Z3fooILf999999EEvi, "void foo<(float)[999999]>(int)");
demangles!(_Z3fooILfaaaaaaEEvi, "void foo<(float)[aaaaaa]>(int)");
demangles!(float_literal_with_decimal, "_Z3fooILf999.999EEvi", "void foo<(float)[999.999]>(int)");
demangles!(_Z3fooILfn65EEvi, "void foo<(float)-[65]>(int)");
demangles!(float_literal_with_negative_sign, "_Z3fooILf-65EEvi", "void foo<(float)[-65]>(int)");

demangles!(_Z3fooILin1EEvv, "void foo<-1>()");
demangles!(_Z3fooILi0EEvv, "void foo<0>()");
demangles!(_Z3fooILin0EEvv, "void foo<-0>()");
demangles!(_Z3fooILi999999EEvv, "void foo<999999>()");
demangles!(_Z3fooILiaaaaaaEEvv, "void foo<aaaaaa>()");
demangles!(int_literal_with_decimal, "_Z3fooILi999.999EEvv", "void foo<999.999>()");

demangles!(_Z3abcrA_l, "abc(long restrict [])");
demangles!(_Z3abcFrA_lvE, "abc(long restrict (()) [])");
demangles!(_Z3abcFrPA_lvE, "abc(long (* restrict()) [])");
demangles!(_Z3abcM3defFPVPFrPivEvE, "abc(int* restrict (* volatile* (def::*)())())");
demangles!(_Z3abcM3defFPVPFrPA_lvEvE, "abc(long (* restrict (* volatile* (def::*)())()) [])");
demangles!(_Z3abcKFA_ivE, "abc(int (() const) [])");
demangles!(_Z3abcFFivElE, "abc(int (long)())");
demangles!(_Z3abcFPFrPivElE, "abc(int* restrict (*(long))())");
demangles!(_Z3abcKFvRSt7ostreamE, "abc(void (std::ostream&) const)");

// Test cases found via differential testing against `c++filt` with `cargo-fuzz`
// and `libFuzzer`.

demangles!(
    _Z5ccc_Z5cccmmmml,
    "ccc_Z(cccmm, unsigned long, unsigned long, long)"
);
demangles!(
    __Z3S_Z3SGffffjjjjjjjjjjzjjjjjjojjjjjjjj,
    "S_Z(SGf, float, float, float, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, ..., unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned __int128, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int)"
);
demangles!(
    __Z3SGfDdedddd,
    "SGf(decimal64, long double, double, double, double, double)"
);
demangles!(
    __ZN6ISiS_Z3b_dE1ES0_7__dIFFFdhl,
    "ISiS_Z::b_d(E, E, __dIFFF, double, unsigned char, long)"
);
