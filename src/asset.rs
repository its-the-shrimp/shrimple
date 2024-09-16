use {
    crate::{utils::{OptionExt, ShortStr}, view::{ByteView, PathView, StrView}},
    anyhow::{Context, Result},
    std::{ffi::OsStr, fs::read_to_string, mem::replace, path::{absolute, Path}},
};

#[derive(Debug, Clone, PartialEq, Eq)]
/// The string stored in the state is the source code
pub enum AssetState {
    Raw,
    Cached { content: ByteView, ext: ShortStr },
    Template(StrView),
    Processed,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Asset {
    /// if `state` is `Cached`, this is the URL from which the file was cached
    pub path: PathView,
    pub state: AssetState,
}

impl Asset {
    pub fn new_template(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let path = path
            .canonicalize()
            .with_context(|| format!("failed to locate {path:?}"))?
            .into();
        Ok(Self { state: AssetState::Template(read_to_string(&path)?.into()), path })
    }

    pub const fn new_cached(path: PathView, content: ByteView, ext: ShortStr) -> Self {
        Self { path, state: AssetState::Cached { content, ext } }
    }

    pub fn new(
        path: impl AsRef<Path>,
        needs_processing: Option<bool>,
        processed_exts: &[impl AsRef<OsStr>],
    ) -> Result<Self> {
        let path = path.as_ref();
        Ok(Self {
            path: absolute(path).with_context(|| format!("failed to locate {path:?}"))?.into(),
            state: match needs_processing {
                Some(true) => AssetState::Template(
                    read_to_string(path)
                        .with_context(|| format!("failed to read contents of {path:?}"))?
                        .into(),
                ),
                Some(false) => AssetState::Raw,
                None => path
                    .extension()
                    .filter(|ext| processed_exts.iter().any(|lhs| lhs.as_ref() == *ext))
                    .try_map(|_| read_to_string(path))?
                    .map_or(AssetState::Raw, |src| AssetState::Template(src.into())),
            },
        })
    }

    pub fn src(&self) -> Option<StrView> {
        match &self.state {
            AssetState::Template(src) => Some(src.clone()),
            _ => None,
        }
    }

    /// if `self` is `Template`, switches it to `Processed` and returns the source code, otherwise
    /// returns None
    pub fn src_for_processing(&mut self) -> Option<StrView> {
        match replace(&mut self.state, AssetState::Processed) {
            AssetState::Template(src) => Some(src),
            prev => {
                self.state = prev;
                None
            },
        }
    }
}

/// `desc` should be a nominal clause, i.e. `parsing`, `template expansion`, etc.
pub fn wrap_error(
    assets: &[Asset],
    error: anyhow::Error,
    at: *const u8,
    desc: &str,
) -> anyhow::Error {
    if let Some((filepath, loc)) = shrimple_parser::utils::locate_in_multiple(
        at,
        assets.iter().map(|s| (&*s.path, s.src().unwrap_or_default())),
    ) {
        error.context(format!("{desc} error at {}:{}", filepath.display(), loc))
    } else {
        error.context(format!("{desc} error at unknown location"))
    }
}
