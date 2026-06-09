#[cfg(feature = "regen-tests")]
fn main() -> anyhow::Result<()> {
    use anyhow::Context;
    use std::fs::{File, read_dir};
    use std::io::{BufWriter, Write};

    let mut test_suite = BufWriter::new(File::create("tests/main.rs")?);
    writeln!(test_suite, "//! Auto-generated from build.rs\n\nmod common;\nuse common::run_test;")?;

    let mut tests =
        read_dir("tests/samples")?.map(|e| e.map(|e| e.path())).collect::<Result<Vec<_>, _>>()?;
    tests.sort();
    for path in tests {
        let name = path.file_name().context("no filename")?.to_str().context("invalid filename")?;
        writeln!(test_suite, "\n#[test]\nfn {name}() {{\n    run_test({path:?});\n}}")?;
    }
    Ok(())
}

#[cfg(not(feature = "regen-tests"))]
fn main() {}
