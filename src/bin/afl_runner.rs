fn main() {
    extern crate afl;
    extern crate cpp_demangle;

    afl::read_stdio_bytes(|bytes| {
        let _ = cpp_demangle::Symbol::new(bytes);
    });
}
