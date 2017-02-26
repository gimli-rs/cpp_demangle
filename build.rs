use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path;

fn get_crate_dir() -> io::Result<path::PathBuf> {
    Ok(path::PathBuf::from(try!(env::var("CARGO_MANIFEST_DIR")
        .map_err(|_| io::Error::new(io::ErrorKind::Other, "no CARGO_MANIFEST_DIR")))))
}

fn get_test_path(file_name: &str) -> io::Result<path::PathBuf> {
    let mut test_path = try!(get_crate_dir());
    test_path.push("tests");
    assert!(test_path.is_dir());
    test_path.push(file_name);
    Ok(test_path)
}

/// Generate tests that ensure that we don't panic when parsing and demangling
/// the seed test cases that we pass to AFL.rs assert (including the failing
/// test cases historically found by AFL.rs).
fn generate_sanity_tests_from_afl_seeds() -> io::Result<()> {
    println!("cargo:rerun-if-changed=in/*");
    println!("cargo:rerun-if-changed=tests/afl_seeds.rs");

    let test_path = try!(get_test_path("afl_seeds.rs"));
    let mut test_file = try!(fs::File::create(test_path));

    try!(writeln!(&mut test_file,
                  "
extern crate cpp_demangle;
use std::fs;
use std::io::Read;
"));

    let mut in_dir = try!(get_crate_dir());
    in_dir.push("in");
    assert!(in_dir.is_dir());

    let entries = try!(fs::read_dir(in_dir));

    for entry in entries {
        let entry = try!(entry);

        let path = entry.path();
        let file_name = try!(path.file_name()
            .ok_or(io::Error::new(io::ErrorKind::Other,
                                  "no file name for AFL.rs seed test case")));

        try!(writeln!(&mut test_file,
                      r#"
#[test]
fn test_afl_seed_{}() {{
    let mut file = fs::File::open("{}").unwrap();
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).unwrap();
    let _ = cpp_demangle::Symbol::new(contents);
    assert!(true, "did not panic when parsing");
}}
"#,
                      file_name.to_string_lossy(),
                      path.to_string_lossy()));
    }

    Ok(())
}

// Ratcheting number that is increased as more libiberty tests start
// passing. Once they are all passing, this can be removed and we can enable all
// of them by default.
const LIBIBERTY_TEST_THRESHOLD: usize = 52;

/// Read `tests/libiberty-demangle-expected`, parse its input mangled symbols,
/// and expected output demangled symbols, and generate test cases for them.
///
/// We do not support all of the options that the libiberty demangler does,
/// therefore we skip tests that use options we do not intend to
/// support. Basically, we only support `--format=gnu-v3` (which is the System V
/// C++ ABI), and none of the legacy C/C++ compiler formats, nor Java/D/etc
/// language symbol mangling.
fn generate_compatibility_tests_from_libiberty() -> io::Result<()> {
    println!("cargo:rerun-if-changed=tests/libiberty-demangle-expected");

    let test_path = try!(get_test_path("libiberty.rs"));
    let _ = fs::remove_file(&test_path);
    let mut test_file = try!(fs::File::create(test_path));

    try!(writeln!(&mut test_file, "extern crate cpp_demangle;"));

    let libiberty_tests = try!(get_test_path("libiberty-demangle-expected"));
    let libiberty_tests = try!(fs::File::open(libiberty_tests));
    let libiberty_tests = io::BufReader::new(libiberty_tests);

    let mut lines = libiberty_tests.lines()
        .filter(|line| {
            line.as_ref()
                .map(|l| !l.starts_with('#'))
                .unwrap_or(true)
        });

    let mut n = 0;

    loop {
        let options = match lines.next() {
            None => break,
            Some(Ok(line)) => line,
            Some(Err(e)) => return Err(e),
        };

        let mangled = match lines.next() {
            Some(Ok(line)) => line,
            None => {
                return Err(io::Error::new(io::ErrorKind::Other,
                                          "expected a line with a mangled symbol"))
            }
            Some(Err(e)) => return Err(e),
        };

        let demangled = match lines.next() {
            Some(Ok(line)) => line,
            None => {
                return Err(io::Error::new(io::ErrorKind::Other,
                                          "expected a line with the demangled symbol"))
            }
            Some(Err(e)) => return Err(e),
        };

        if options.find("--no-params").is_some() {
            // This line is the expected demangled output without function and
            // template parameters, but we don't currently have such an option
            // in `cpp_demangle`, so just consume and ignore the line.
            match lines.next() {
                Some(Ok(_)) => {}
                None => {
                    return Err(io::Error::new(io::ErrorKind::Other,
                                              "expected a line with the demangled symbol without parameters"))
                }
                Some(Err(e)) => return Err(e),
            }
        }

        // Skip tests for unsupported languages or options.
        if options.find("--format=gnu-v3").is_none() ||
           options.find("--is-v3-ctor").is_some() ||
           options.find("--is-v3-dtor").is_some() ||
           options.find("--ret-postfix").is_some() {
            continue;
        }

        try!(writeln!(test_file,
                      r###"
{}
#[test]
fn test_libiberty_demangle_{}_() {{
    let mangled = br#"{}"#;
    println!("Parsing mangled symbol: {{}}", String::from_utf8_lossy(mangled));

    let sym = cpp_demangle::Symbol::new(&mangled[..])
        .expect("should parse mangled symbol");

    let expected = r#"{}"#;
    let actual = format!("{{}}", sym);
    println!("     Expect demangled symbol: {{}}", expected);
    println!("Actually demangled symbol as: {{}}", actual);

    assert_eq!(expected, actual);
}}
"###,
                      if n <= LIBIBERTY_TEST_THRESHOLD {
                          ""
                      } else {
                          r###"#[cfg(feature = "run_libiberty_tests")]"###
                      },
                      n,
                      mangled.trim(),
                      demangled.trim()));

        n += 1;
    }

    Ok(())
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    generate_sanity_tests_from_afl_seeds()
        .expect("should generate sanity tests from AFL.rs seed test cases");

    generate_compatibility_tests_from_libiberty()
        .expect("should generate compatibility tests from tests/libiberty-demangle-expected");
}
