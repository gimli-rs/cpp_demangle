#![cfg_attr(feature = "fuzz", feature(plugin))]
#![cfg_attr(feature = "fuzz", plugin(afl_plugin))]

#[cfg(not(feature = "fuzz"))]
fn main() {
    println!("Not built with a rustc nightly and `--features fuzz`; aborting.");
}

#[cfg(feature = "fuzz")]
fn main() {
    extern crate afl;
    extern crate cpp_demangle;

    afl::handle_bytes(|bytes| {
        let _ = cpp_demangle::Symbol::new(bytes);
    });
}
