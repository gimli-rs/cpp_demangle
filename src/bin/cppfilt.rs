// For clippy.
#![allow(unknown_lints)]

extern crate cpp_demangle;

// For command line integration
#[macro_use]
extern crate clap;

use cpp_demangle::BorrowedSymbol;
use std::io::{self, BufRead, Write};
use std::process;
use clap::App;

/// Find the index of the first (potential) occurrence of a mangled C++ symbol
/// in the given `haystack`.
#[allow(needless_range_loop)]
fn find_mangled(haystack: &[u8]) -> Option<usize> {
    if haystack.is_empty() {
        return None;
    }

    for i in 0..haystack.len() - 1 {
        if haystack[i] == b'_' {
            let next = haystack[i + 1];
            if next == b'Z' || next == b'_' && haystack.get(i + 2) == Some(&b'Z') {
                return Some(i);
            }
        }
    }

    None
}

/// Print the given `line` to `out`, with all mangled C++ symbols replaced with
/// their demangled form.
fn demangle_line<W>(out: &mut W, line: &[u8]) -> io::Result<()>
    where W: Write
{
    let mut line = line;

    while let Some(idx) = find_mangled(line) {
        try!(write!(out, "{}", String::from_utf8_lossy(&line[..idx])));

        if let Ok((sym, tail)) = BorrowedSymbol::with_tail(&line[idx..]) {
            try!(write!(out, "{}", sym));
            line = tail;
        } else {
            try!(write!(out, "_Z"));
            line = &line[2..];
        }
    }

    write!(out, "{}", String::from_utf8_lossy(line))
}

/// Print all the lines from the given `input` to `out`, with all mangled C++
/// symbols replaced with their demangled form.
fn demangle_all<R, W>(input: &mut R, out: &mut W) -> io::Result<()>
    where R: BufRead,
          W: Write
{
    let mut buf = vec![];

    while try!(input.read_until(b'\n', &mut buf)) > 0 {
        try!(demangle_line(out, &buf[..]));
        buf.clear();
    }

    Ok(())
}

fn main() {
    let _ = App::new("cppfilt")
        .version(crate_version!())
        .author(crate_authors!())
        .about("A c++filt clone as an example of how to use the cpp_demangle crate!")
        .get_matches();

    let stdin = io::stdin();
    let mut stdin = stdin.lock();

    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    let stderr = io::stderr();
    let mut stderr = stderr.lock();

    let code = match demangle_all(&mut stdin, &mut stdout) {
        Ok(_) => 0,
        Err(e) => {
            let _ = writeln!(&mut stderr, "error: {}", e);
            1
        }
    };

    process::exit(code);
}
