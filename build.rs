use std::env;
use std::fs;
use std::io::{self, Write};
use std::path;

// Generate tests that ensure that we don't panic when parsing and demangling
// the seed test cases that we pass to AFL.rs assert (including the failing test
// cases historically found by AFL.rs).
fn generate_sanity_tests_from_afl_seeds() -> io::Result<()> {
    println!("cargo:rerun-if-changed=in/*");
    println!("cargo:rerun-if-changed=tests/afl_seeds.rs");

    let crate_dir = try!(env::var("CARGO_MANIFEST_DIR")
        .map_err(|_| io::Error::new(io::ErrorKind::Other, "no CARGO_MANIFEST_DIR")));

    let mut test_path = path::PathBuf::from(&crate_dir);
    test_path.push("tests");
    let _ = fs::create_dir(&test_path);
    test_path.push("afl_seeds.rs");
    let mut test_file = try!(fs::File::create(test_path));

    try!(writeln!(&mut test_file, "
extern crate cpp_demangle;
use std::fs;
use std::io::Read;
"));

    let mut in_dir = path::PathBuf::from(crate_dir);
    in_dir.push("in");
    assert!(in_dir.is_dir());

    let entries = try!(fs::read_dir(in_dir));

    for entry in entries {
        let entry = try!(entry);

        let path = entry.path();
        let file_name = try!(path
            .file_name()
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

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    generate_sanity_tests_from_afl_seeds()
        .expect("should generate sanity tests from AFL.rs seed test cases");
}
