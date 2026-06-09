use {
    crate::{
        mime::{self, path_extension},
        utils::{BoolExt, InlineStr, Result, assume_static_mut, copy, default},
        view::{OsStrView, StrView},
    },
    anyhow::{Context, Error, bail},
    futures_util::TryFutureExt,
    reqwest::{Client as AsyncClient, blocking::Client as SyncClient},
    shrimple_parser::utils::{Location, locate_in_multiple},
    std::{
        collections::HashMap, ffi::OsStr, fmt::Display, fs::read_to_string, mem::replace,
        panic::resume_unwind, path::Path, str::FromStr,
    },
    tokio::{
        fs::{File, create_dir_all, hard_link, remove_file, write},
        io::AsyncWriteExt,
        task::JoinSet,
        try_join,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssetState {
    /// An asset that should be copied as is
    Raw,
    /// A template not yet compiled
    Template { src: StrView },
    /// A template already compiled
    Compiled { src: StrView, res: StrView },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssetCategory {
    Raw,
    Template,
    Document,
    HtmlDocument,
}

impl FromStr for AssetCategory {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        Ok(match s {
            "raw" => Self::Raw,
            "template" => Self::Template,
            "document" => Self::Document,
            "htmldocument" => Self::HtmlDocument,
            _unknown => bail!("unknown asset category `{s}`"),
        })
    }
}

impl AssetCategory {
    /// Some asset categories may force a certain file extension
    pub const fn ext(self) -> Option<&'static str> {
        match self {
            Self::HtmlDocument => Some("html"),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Asset {
    pub path: StrView,
    pub ext: InlineStr,
    pub url: Option<StrView>,
    pub state: AssetState,
    pub category: AssetCategory,
}

impl Asset {
    pub const fn template_src(&self) -> Option<&StrView> {
        match &self.state {
            AssetState::Template { src } => Some(src),
            _ => None,
        }
    }

    pub fn src(&self) -> Option<StrView> {
        match &self.state {
            AssetState::Template { src } | AssetState::Compiled { src, .. } => Some(src.clone()),
            AssetState::Raw => None,
        }
    }

    pub fn path_or_url(&self) -> &StrView {
        self.url.as_ref().unwrap_or(&self.path)
    }

    pub fn set_compilation_result(&mut self, res: StrView) {
        self.state = match replace(&mut self.state, AssetState::Raw) {
            AssetState::Template { src } => AssetState::Compiled { src, res },
            other => other,
        };
    }
}

pub struct AssetManager {
    assets: Vec<Asset>,
    ext2category: HashMap<OsStrView, AssetCategory>,
    http_client: SyncClient,
}

impl Default for AssetManager {
    fn default() -> Self {
        Self {
            assets: default(),
            ext2category: HashMap::from([
                ("html".into(), AssetCategory::HtmlDocument),
                ("md".into(), AssetCategory::HtmlDocument),
                ("svg".into(), AssetCategory::Document),
                ("css".into(), AssetCategory::Template),
            ]),
            http_client: default(),
        }
    }
}

impl AssetManager {
    pub fn assets(&self) -> impl Iterator<Item = &Asset> + Clone {
        self.assets.iter()
    }

    pub fn asset_category_by_ext(&self, ext: impl AsRef<OsStr>) -> AssetCategory {
        self.ext2category.get(ext.as_ref()).map_or(AssetCategory::Raw, copy)
    }

    pub fn asset_category(&self, path: impl AsRef<Path>) -> Option<AssetCategory> {
        path.as_ref().extension().map(|ext| self.asset_category_by_ext(ext))
    }

    pub fn locate(&self, ptr: *const u8) -> Option<(StrView, Location)> {
        locate_in_multiple(
            ptr,
            self.assets.iter().map(|a| (a.path.clone(), a.src().unwrap_or_default())),
        )
    }

    pub fn register_asset_ext(&mut self, ext: OsStrView, category: AssetCategory) {
        self.ext2category.insert(ext, category);
    }

    /// The function will return the same value until `set_compilation_result` is called
    pub fn next_uncompiled_asset(&self) -> Option<(StrView, Asset)> {
        self.assets
            .iter()
            .find_map(|asset| asset.template_src().map(|src| (src.clone(), asset.clone())))
    }

    pub fn save_compilation_result(&mut self, res: impl Display) {
        if let Some(asset) = self.assets.iter_mut().find(|asset| asset.template_src().is_some()) {
            asset.set_compilation_result(format!("{res:#}").into());
        }
    }

    /// Returns (content, file extension)
    ///
    /// `content` is None if `get_content` is false
    fn get_asset_metadata(
        &self,
        path: &str,
        is_local: bool,
        get_content: bool,
    ) -> Result<(Option<StrView>, InlineStr)> {
        Ok(if is_local {
            let ext = path_extension(path)?;
            let content = get_content
                .then_try(|| read_to_string(path))
                .with_context(|| format!("failed to get the contents of {path:?}"))?
                .map(Into::into);
            (content, ext)
        } else if get_content {
            let response = self.http_client.get(path).send()?;
            let ext = mime::remote_file_ext(&response)?;
            let content = response.text()?.into();
            (Some(content), ext)
        } else {
            let response = self.http_client.head(path).send()?;
            let ext = mime::remote_file_ext(&response)?;
            (None, ext)
        })
    }

    pub fn add_asset(
        &mut self,
        path_or_url: StrView,
        is_local: bool,
        category: Option<AssetCategory>,
    ) -> Result<&mut Asset> {
        if let Some(res) = self.assets.iter_mut().find(|asset| asset.path_or_url() == &path_or_url)
        {
            // SAFETY: the fn signature assets correctness, just circumventing borrow checker
            // errors
            return Ok(unsafe { assume_static_mut(res) });
        }

        let category = category.or_else(|| self.asset_category(&path_or_url));
        let ext;
        let src;
        (src, ext) = self.get_asset_metadata(
            &path_or_url,
            is_local,
            category.is_some_and(|c| c != AssetCategory::Raw),
        )?;
        let state = src.map_or(AssetState::Raw, |src| AssetState::Template { src });
        let mut path = path_or_url;
        let mut url = None;
        if !is_local {
            url = Some(path);
            let n_remote_assets = self.assets.iter().filter(|asset| asset.url.is_some()).count();
            path = format!("cached/{n_remote_assets}.{ext}").into();
        }

        Ok(self.assets.push_mut(Asset {
            category: category.unwrap_or_else(|| self.asset_category_by_ext(&*ext)),
            path,
            ext,
            url,
            state,
        }))
    }

    pub async fn write_to_disk(&mut self, dst_root: impl AsRef<Path>) -> Result {
        let dst_root = dst_root.as_ref();
        let http_client = AsyncClient::new();

        let mut tasks = JoinSet::<Result>::new();
        for asset in self.assets.drain(..) {
            let mut dst = dst_root.join(&asset.path);
            if let Some(dst_ext) = asset.category.ext() {
                dst.set_extension(dst_ext);
            }

            match &asset.state {
                AssetState::Raw => {
                    if let Some(url) = asset.url {
                        let http_client = http_client.clone();
                        tasks.spawn(async move {
                            if let Some(dst_dir) = dst.parent() {
                                create_dir_all(dst_dir)
                                    .await
                                    .with_context(|| format!("failed to create {dst_dir:?}"))?;
                            }

                            let (mut response, mut dst_file) = try_join!(
                                http_client.get(&*url).send().map_err(anyhow::Error::new),
                                File::create(&dst).map_err(anyhow::Error::new),
                            )?;
                            while let Some(mut chunk) = response.chunk().await? {
                                dst_file.write_all_buf(&mut chunk).await?;
                            }

                            Ok(())
                        });
                    } else {
                        let src = asset.path.clone();
                        let dst = dst.clone();
                        tasks.spawn(async move {
                            if let Some(dst_dir) = dst.parent() {
                                create_dir_all(dst_dir)
                                    .await
                                    .with_context(|| format!("failed to create {dst_dir:?}"))?;
                            }

                            if dst.exists() {
                                remove_file(&dst).await.with_context(|| {
                                    format!("failed to remove {dst:?} to link it to {src:?}")
                                })?;
                            }

                            hard_link(&src, &dst)
                                .await
                                .with_context(|| format!("failed to link {src:?} to {dst:?}"))
                        });
                    }
                }

                AssetState::Template { .. } => {
                    bail!("bug: tried to save results to disk before compiling all the assets")
                }

                AssetState::Compiled { res, .. } => {
                    let res = res.clone();
                    tasks.spawn(async move {
                        if let Some(dst_dir) = dst.parent() {
                            create_dir_all(dst_dir)
                                .await
                                .with_context(|| format!("failed to create {dst_dir:?}"))?;
                        }
                        write(&dst, &res)
                            .await
                            .with_context(|| format!("failed to write to {dst:?}"))?;
                        Ok(())
                    });
                }
            }
        }

        while let Some(res) = tasks.join_next().await {
            match res {
                Ok(res) => res?,
                Err(err) if err.is_panic() => resume_unwind(err.into_panic()),
                Err(err) => panic!("{err}"),
            }
        }

        Ok(())
    }
}
