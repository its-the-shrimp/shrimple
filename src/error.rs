use anyhow::{anyhow, Context, Result};
use nom::Needed;
use std::path::{Path, PathBuf};

/// - Locates a pointer in the source code `src`, by extension, adds such info to errors.
/// - Locates files relatively to the source file
#[derive(Clone, Copy)]
pub struct Locator<'ctx> {
    src: &'ctx str,
    filename: &'ctx Path,
    dir: &'ctx Path,
}

impl<'ctx> Locator<'ctx> {
    /// `filename` must be absolute or relative to the current directory
    pub fn new(src: &'ctx str, filename: &'ctx Path) -> Result<Self> {
        Ok(Self { src, filename, dir: filename.parent().context("empty filename provided")? })
    }

    /// the return values are [line, column]
    fn ptr_to_loc(src: &str, ptr: *const u8) -> [usize; 2] {
        let offset =
            (ptr as usize).checked_sub(src.as_ptr() as usize).map_or(0, |x| x.min(src.len() - 1));
        let [mut line, mut column] = [1, 0];
        for byte in unsafe { src.get_unchecked(..=offset).bytes() } {
            if byte == b'\n' {
                line += 1;
                column = 0
            } else {
                column += 1
            }
        }
        [line, column]
    }

    pub fn locate_ptr(&self, ptr: *const u8) -> String {
        let [line, column] = Self::ptr_to_loc(self.src, ptr);
        format!("{}:{line}:{column}", self.filename.display())
    }

    pub fn locate_path(&self, path: impl AsRef<Path>) -> PathBuf {
        let path = path.as_ref();
        if !path.starts_with(self.dir) {
            return self.dir.join(path)
        }
        path.to_owned()
    }

    /// Returns `path` relative to the path of the locator, or an error if `path` isn't related to
    /// it.
    pub fn as_relative<'path>(&self, path: &'path Path) -> Result<&'path Path> {
        path.strip_prefix(self.dir)
            .with_context(|| format!("{path:?} isn't relative to the directory"))
    }

    /// `desc` should be a nominal clause, i.e. `parsing`, `template expansion`, etc.
    /// if `loc` is `None`, location at the first byte of the input is assumed
    pub fn wrap(&self, error: anyhow::Error, loc: Option<*const u8>, desc: &str) -> anyhow::Error {
        let [l, c] = loc.map_or([1, 0], |loc| Self::ptr_to_loc(self.src, loc));
        anyhow!(error).context(format!("{desc} error at {}:{l}:{c}", self.filename.display()))
    }

    /// Exists in part because `nom::Err`'s `Display` impl is nonsense
    pub fn wrap_nom_error(&self, error: nom::Err<nom::error::Error<&'ctx str>>) -> anyhow::Error {
        match error {
            nom::Err::Incomplete(Needed::Size(u)) => anyhow!("Parsing requires {u} bytes/chars"),
            nom::Err::Incomplete(Needed::Unknown) => anyhow!("Parsing requires more data"),
            nom::Err::Failure(e) => {
                let [l, c] = Self::ptr_to_loc(self.src, e.input.as_ptr());
                anyhow!("Parsing failure at {}:{l}:{c}:\n{e}", self.filename.display())
            }
            nom::Err::Error(e) => {
                let [l, c] = Self::ptr_to_loc(self.src, e.input.as_ptr());
                anyhow!("Parsing error at {}:{l}:{c}:\n{e}", self.filename.display())
            }
        }
    }
}
