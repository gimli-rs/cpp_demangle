#![no_main]
#![feature(ascii_ctype)]

#[macro_use]
extern crate libfuzzer_sys;
extern crate cpp_demangle;

use std::ascii::AsciiExt;
use std::io::Write;
use std::process::{Command, Output, Stdio};
use std::str;

macro_rules! ignore_err {
    ( $e:expr ) => {
        match $e {
            Ok(v) => v,
            Err(_) => return,
        }
    }
}

fuzz_target!(|data: &[u8]| {
    // We only want one one symbol at a time. We don't want to test `c++filt`'s
    // pass-through behavior of data that isn't a mangled C++ symbol.
    let data = ignore_err!(str::from_utf8(data));
    let data = data.trim();
    let data: String = data.chars()
        .take_while(|&c| c.is_ascii_graphic())
        .collect();
    if !data.starts_with("__Z") {
        return;
    }

    let mut child = ignore_err!(Command::new("c++filt")
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .spawn());

    let mut stdin = child.stdin.take().unwrap();
    ignore_err!(writeln!(&mut stdin, "{}", data));
    drop(stdin);

    let Output { status, stdout, .. } = ignore_err!(child.wait_with_output());
    if !status.success() {
        return;
    }

    let cppfilt_output = ignore_err!(String::from_utf8(stdout));
    if cppfilt_output.trim() == data {
        // `c++filt` didn't demangle any symbols, so skip this input.
        return;
    }

    let sym = ignore_err!(cpp_demangle::Symbol::new(data));
    let demangled = sym.to_string();
    assert_eq!(demangled.trim(), cppfilt_output.trim(),
               "Should demangle the input the same as `c++filt` does");
});
