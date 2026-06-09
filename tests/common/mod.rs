use std::{
    collections::BTreeMap,
    env::var,
    ffi::OsString,
    fmt::Write,
    fs::{self, DirEntry},
    io,
    path::Path,
    process::Command,
};

pub fn run_test(src_dir: impl AsRef<Path>) {
    let shrimple_name = env!("CARGO_BIN_EXE_shrimple");
    let src_dir = src_dir.as_ref();
    let src_root = src_dir.join("index.html");
    let dst_dir = src_dir.join("dist");
    let expected_dist_dir = src_dir.join("expected-dist");
    let mut shrimple_cmd = Command::new(shrimple_name);
    shrimple_cmd.arg(src_root);
    shrimple_cmd.arg("-o");
    shrimple_cmd.arg(&dst_dir);
    let shrimple_res = shrimple_cmd.output().expect("run `shrimple`");

    assert!(
        shrimple_res.status.success(),
        "`shrimple` completed with exit code {status}\n\
          command: {shrimple_cmd:#?}\n\
          stdout:\n\
          {stdout}\n\
          stderr:\n\
          {stderr}",
        status = shrimple_res.status,
        stdout = String::from_utf8_lossy(&shrimple_res.stdout),
        stderr = String::from_utf8_lossy(&shrimple_res.stderr),
    );

    if var("BLESS").is_ok() || !expected_dist_dir.exists() {
        copy_dir_all(&dst_dir, &expected_dist_dir).expect("copy `dist` to `expected-dist`");
        return;
    }

    let diffs =
        compare_dirs(&dst_dir, &expected_dist_dir).expect("compare `dist` & `expected-dist`");
    assert!(diffs.is_empty(), "The resulting website didn't meet expectations:\n{diffs}");
}

fn copy_dir_all(src: &Path, dst: &Path) -> io::Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let dst_path = dst.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_dir_all(&entry.path(), &dst_path)?;
        } else {
            fs::copy(entry.path(), dst_path)?;
        }
    }
    Ok(())
}

fn compare_dirs(a: &Path, b: &Path) -> io::Result<String> {
    let mut diffs = String::new();
    compare_dirs_inner(a, b, Path::new(""), &mut diffs).map(|_| diffs)
}

fn compare_dirs_inner(a: &Path, b: &Path, rel: &Path, diffs: &mut String) -> io::Result<()> {
    let read_entries = |dir: &Path| -> io::Result<BTreeMap<OsString, DirEntry>> {
        Ok(fs::read_dir(dir)?.flatten().map(|e| (e.file_name(), e)).collect())
    };

    let a_entries = read_entries(a)?;
    let b_entries = read_entries(b)?;

    for (name, a_entry) in &a_entries {
        let rel_path = rel.join(name);
        match b_entries.get(name) {
            None => {
                _ = writeln!(diffs, "only in dist: {}", rel_path.display());
            }
            Some(b_entry) => {
                let a_path = a_entry.path();
                let b_path = b_entry.path();
                if a_entry.file_type().is_ok_and(|ft| ft.is_dir()) {
                    compare_dirs_inner(&a_path, &b_path, &rel_path, diffs)?;
                } else {
                    let a_content = fs::read(&a_path).expect("read dist file");
                    let b_content = fs::read(&b_path).expect("read expected-dist file");
                    if a_content != b_content {
                        let a_content = String::from_utf8_lossy(&a_content);
                        let b_content = String::from_utf8_lossy(&b_content);
                        _ = writeln!(
                            diffs,
                            "files differ: {a_path:?} & {b_path:?}\n\n{a_content:?}\n\n{b_content:?}"
                        );
                    }
                }
            }
        }
    }

    for name in b_entries.keys() {
        if !a_entries.contains_key(name) {
            _ = writeln!(diffs, "only in expected-dist: {}", rel.join(name).display());
        }
    }

    Ok(())
}
