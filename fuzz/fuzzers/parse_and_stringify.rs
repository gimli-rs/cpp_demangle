#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate cpp_demangle;

fuzz_target!(|data: &[u8]| {
    if let Ok(sym) = cpp_demangle::Symbol::new(data) {
        let _ = sym.to_string();
    }
});
