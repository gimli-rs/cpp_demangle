extern crate cpp_demangle;
extern crate diff;

use std::fs;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::process;

const NUMBER_OF_LIBXUL_SYMBOLS: usize = 274_346;

// These counts should only go up!
const NUMBER_OF_LIBXUL_SYMBOLS_THAT_PARSE: usize = 274_346;
const NUMBER_OF_LIBXUL_SYMBOLS_THAT_DEMANGLE: usize = 274_346;

fn get_cppfilt() -> &'static str {
    if cfg!(not(target_os = "macos")) {
        return "c++filt";
    }

    // Prefer `gc++filt` (from the homebrew binutils package) since it is built
    // with a newer libiberty than the system `c++filt` (and maybe the system
    // `c++filt` will be backed by the LLVM demangler one day).
    match process::Command::new("gc++filt").spawn() {
        Ok(mut child) => {
            child.kill().expect("should kill child");
            child.wait().expect("should wait on child");
            "gc++filt"
        }
        Err(_) => "c++filt",
    }
}

#[test]
fn libxul_symbols_demangle() {
    let mut total = 0;
    let mut num_parsed = 0;
    let mut num_demangled = 0;
    let mut num_match_libiberty = 0;

    let libxul_txt_file = fs::File::open(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/libxul.txt"))
        .expect("should open libxul.txt");
    let mut libxul_txt_file = BufReader::new(libxul_txt_file);

    let log_file = fs::File::create(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/libxul.log"))
        .expect("should create log file");
    let mut log_file = BufWriter::new(log_file);

    let mut line = Vec::new();
    let mut demangled = Vec::new();
    let mut libiberty_sym = Vec::new();

    let which_cppfilt = get_cppfilt();
    let mut cppfilt = process::Command::new(which_cppfilt)
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .spawn()
        .expect("should spawn c++filt");
    let mut cppfilt_stdin = cppfilt.stdin.take().unwrap();
    let mut cppfilt_stdout = BufReader::new(cppfilt.stdout.take().unwrap());

    while {
        line.clear();
        let bytes_read = libxul_txt_file
            .read_until(b'\n', &mut line)
            .expect("should read line");
        bytes_read > 0
    } {
        total += 1;

        // Grab the next symbol from libxul.txt.
        let line_len = line.len();
        assert!(line_len > 2);
        let mut line = if line[line_len - 1] == b'\n' {
            &line[..line_len - 1]
        } else {
            &line[..]
        };

        // libxul.txt was generated on macOS, and so it has the double
        // underscore ("__Z"). Remove the first underscore if needed.
        if line[0] == b'_' && (cfg!(not(target_os = "macos")) || which_cppfilt == "gc++filt") {
            line = &line[1..];
        }

        // Parse the symbol.
        if let Ok(sym) = cpp_demangle::BorrowedSymbol::new(line) {
            num_parsed += 1;

            // Demangle the symbol.
            demangled.clear();
            if write!(&mut demangled, "{}", sym).is_ok() {
                num_demangled += 1;

                // Finally, we are going to have `c++filt` demangle the
                // symbol. Write the line into `c++filt`.
                cppfilt_stdin
                    .write_all(&line)
                    .expect("should write line contents into c++filt");
                cppfilt_stdin
                    .write_all(b"\n")
                    .expect("should write newline into c++filt");
                cppfilt_stdin.flush().expect("should flush c++filt stdin");

                // Read its demangled version back out.
                libiberty_sym.clear();
                cppfilt_stdout
                    .read_until(b'\n', &mut libiberty_sym)
                    .expect("should read line from c++filt");
                // Drop the "\n".
                libiberty_sym.pop();

                // Compare our demangling to libiberty's.
                if libiberty_sym == demangled {
                    num_match_libiberty += 1;
                } else {
                    writeln!(
                        &mut log_file,
                        "failed to match libiberty's demangling: {}",
                        String::from_utf8_lossy(line)
                    ).unwrap();
                    writeln!(
                        &mut log_file,
                        "           ...we demangled to: {}",
                        String::from_utf8_lossy(&demangled)
                    ).unwrap();
                    writeln!(
                        &mut log_file,
                        "    ...libiberty demangled to: {}",
                        String::from_utf8_lossy(&libiberty_sym)
                    ).unwrap();
                }
            } else {
                writeln!(
                    &mut log_file,
                    "failed to demangle libxul symbol: {}",
                    String::from_utf8_lossy(line)
                ).unwrap();
            }
        } else {
            writeln!(
                &mut log_file,
                "failed to parse libxul symbol: {}",
                String::from_utf8_lossy(line)
            ).unwrap();
        }
    }

    writeln!(
        &mut log_file,
        "================================================================"
    ).unwrap();
    writeln!(
        &mut log_file,
        "Total number of libxul symbols:                       {}",
        total
    ).unwrap();
    writeln!(
        &mut log_file,
        "Number of libxul symbols parsed:                      {} ({:.2}%)",
        num_parsed,
        num_parsed as f64 / total as f64 * 100.0
    ).unwrap();
    writeln!(
        &mut log_file,
        "Number of libxul symbols demangled:                   {} ({:.2}%)",
        num_demangled,
        num_demangled as f64 / total as f64 * 100.0
    ).unwrap();
    writeln!(
        &mut log_file,
        "Number of libxul symbols demangled same as libiberty: {} ({:.2}%)",
        num_match_libiberty,
        num_match_libiberty as f64 / num_demangled as f64 * 100.0
    ).unwrap();

    assert!(num_match_libiberty <= num_demangled);
    assert!(num_demangled <= num_parsed);
    assert!(num_parsed <= total);

    assert_eq!(
        NUMBER_OF_LIBXUL_SYMBOLS,
        total,
        "should have expected number of total symbols"
    );
    assert_eq!(
        NUMBER_OF_LIBXUL_SYMBOLS_THAT_PARSE,
        num_parsed,
        "should have expected number of symbols that we can parse"
    );
    assert_eq!(
        NUMBER_OF_LIBXUL_SYMBOLS_THAT_DEMANGLE,
        num_demangled,
        "should have expected number of symbols that we can demangle"
    );

    // Note: we don't assert any number of symbols that demangle the same as
    // libiberty since this depends on the libiberty version that the `c++filt`
    // on this system is using, which varies between CI and dev machines and
    // etc...
}
