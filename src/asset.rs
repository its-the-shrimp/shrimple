use crate::utils::{OptionExt, ShortStr};
use anyhow::{Context, Result};
use std::{ffi::OsStr, fs::read_to_string, path::Path};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// the string stored in the state is the source code
pub enum AssetState {
    Raw,
    Cached{content: &'static [u8], ext: ShortStr},
    Template(&'static str),
    Processed(&'static str),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Asset {
    /// if `state` is `Cached`, this is the URL from which the file was cached
    pub path: &'static Path,
    pub state: AssetState,
}

impl Asset {
    pub fn new_template(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let path: &'static Path = Box::leak(
            path.canonicalize()
                .with_context(|| format!("failed to locate {path:?}"))?
                .into_boxed_path(),
        );
        Ok(Self {
            path,
            state: AssetState::Template(read_to_string(path)?.leak()),
        })
    }

    pub fn new_cached(url: impl AsRef<str>, content: &'static [u8], ext: ShortStr) -> Self {
        Self {
            path: Path::new(url.as_ref().to_owned().leak()),
            state: AssetState::Cached{content, ext}
        }
    }

    pub fn new(
        path: impl AsRef<Path>,
        needs_processing: Option<bool>,
        processed_exts: &[&OsStr],
    ) -> Result<Self> {
        let path = path.as_ref();
        Ok(Self {
            path: Box::leak(
                path.canonicalize()
                    .with_context(|| format!("failed to locate {path:?}"))?
                    .into_boxed_path(),
            ),
            state: match needs_processing {
                Some(true) => AssetState::Template(
                    read_to_string(path)
                        .with_context(|| format!("failed to read contents of {path:?}"))?
                        .leak(),
                ),
                Some(false) => AssetState::Raw,
                None => path
                    .extension()
                    .filter(|ext| processed_exts.contains(ext))
                    .try_map(|_| read_to_string(path))?
                    .map_or(AssetState::Raw, |src| AssetState::Template(src.leak())),
            },
        })
    }

    pub const fn src(&self) -> Option<&'static str> {
        match &self.state {
            AssetState::Raw | AssetState::Cached{..} => None,
            AssetState::Template(src) | AssetState::Processed(src) => Some(src),
        }
    }

    /// if `self` is `Template`, switches it to `Processed` and returns the source code, otherwise
    /// returns None
    pub fn src_for_processing(&mut self) -> Option<&'static str> {
        match self.state {
            AssetState::Template(src) => {
                self.state = AssetState::Processed(src);
                Some(src)
            }
            _ => None,
        }
    }
}

/// `desc` should be a nominal clause, i.e. `parsing`, `template expansion`, etc.
pub fn wrap_error(
    assets: &[Asset],
    error: anyhow::Error,
    at: *const u8,
    desc: &str
) -> anyhow::Error {
    if let Some((filepath, loc)) = shrimple_parser::utils::locate_in_multiple(
        at,
        assets.iter().map(|s| (s.path, s.src().unwrap_or("")))
    ) {
        error.context(format!("{desc} error at {}:{}", filepath.display(), loc))
    } else {
        error.context(format!("{desc} error at unknown location"))
    }
}
