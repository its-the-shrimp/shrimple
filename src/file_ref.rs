use crate::utils::{os_str, OptionExt};
use anyhow::{anyhow, Context, Result};
use nom::Needed;
use std::{ffi::OsStr, fs::read_to_string, path::Path};

/// file formats that are processed by default as template files
const PROCESSED_FORMATS: [&OsStr; 2] = [os_str("css"), os_str("html")];

/// the return values are `[line, column]`
pub fn ptr_to_loc(src: &str, ptr: *const u8) -> [usize; 2] {
    if src.is_empty() {
        return [0, 0];
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// the string stored in the state is the source code
pub enum FileReprState {
    Raw,
    Template(&'static str),
    Processed(&'static str),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct FileRepr {
    pub path: &'static Path,
    pub state: FileReprState,
}

impl FileRepr {
    pub fn new(path: impl AsRef<Path>, needs_processing: Option<bool>) -> Result<Self> {
        let path = path.as_ref();
        Ok(Self {
            path: Box::leak(
                path.canonicalize()
                    .with_context(|| format!("failed to locate {path:?}"))?
                    .into_boxed_path(),
            ),
            state: match needs_processing {
                Some(true) => FileReprState::Template(
                    read_to_string(path)
                        .with_context(|| format!("failed to read contents of {path:?}"))?
                        .leak(),
                ),
                Some(false) => FileReprState::Raw,
                None => path
                    .extension()
                    .filter(|ext| PROCESSED_FORMATS.contains(ext))
                    .try_map(|_| read_to_string(path))?
                    .map_or(FileReprState::Raw, |src| FileReprState::Template(src.leak())),
            },
        })
    }

    pub fn is_raw(&self) -> bool {
        matches!(self.state, FileReprState::Raw)
    }

    pub fn src(&self) -> Option<&'static str> {
        match &self.state {
            FileReprState::Raw => None,
            FileReprState::Template(src) | FileReprState::Processed(src) => Some(src),
        }
    }

    /// if `self` is `Template`, switches it to `Processed` and returns the source code, otherwise
    /// returns None
    pub fn src_for_processing(&mut self) -> Option<&'static str> {
        match self.state {
            FileReprState::Template(src) => {
                self.state = FileReprState::Processed(src);
                Some(src)
            }
            _ => None,
        }
    }

    /// `ptr` is saturated around the ends of the source code, thus, e.g. NULL will be treated as
    /// the first byte of the source, `NULL - 1` - as the last
    pub fn locate(&self, ptr: *const u8) -> String {
        if let Some(src) = self.src() {
            let [line, column] = ptr_to_loc(src, ptr);
            format!("{}:{line}:{column}", self.path.display())
        } else {
            self.path.to_string_lossy().into_owned()
        }
    }

    /// `desc` should be a nominal clause, i.e. `parsing`, `template expansion`, etc.
    pub fn wrap(&self, error: anyhow::Error, ptr: *const u8, desc: &str) -> anyhow::Error {
        anyhow!(error).context(format!("{desc} error at {}", self.locate(ptr)))
    }

    /// Exists in part because `nom::Err`'s `Display` impl is nonsense
    pub fn wrap_nom_error(&self, error: nom::Err<nom::error::Error<&str>>) -> anyhow::Error {
        match error {
            nom::Err::Incomplete(Needed::Size(u)) => anyhow!("Parsing requires {u} bytes/chars"),
            nom::Err::Incomplete(Needed::Unknown) => anyhow!("Parsing requires more data"),
            nom::Err::Failure(e) => {
                anyhow!("Parsing failure at {}:\n{e}", self.locate(e.input.as_ptr()))
            }
            nom::Err::Error(e) => {
                anyhow!("Parsing error at {}:\n{e}", self.locate(e.input.as_ptr()))
            }
        }
    }
}
