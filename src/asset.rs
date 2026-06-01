use {
    crate::{
        ast::XmlNode,
        mime::{self, path_extension},
        utils::{BoolExt, InlineStr, Result, assume_static_mut, default},
        view::{OsStrView, StrView},
    },
    anyhow::{Context, bail},
    futures_util::TryFutureExt,
    reqwest::{Client as AsyncClient, blocking::Client as SyncClient},
    shrimple_parser::utils::{Location, locate_in_multiple},
    std::{
        ffi::OsStr, fmt::Display, fs::read_to_string, mem::replace, panic::resume_unwind,
        path::Path,
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Asset {
    pub path: StrView,
    pub ext: InlineStr,
    pub url: Option<StrView>,
    pub state: AssetState,
}

impl Asset {
    pub fn is_template(path: impl AsRef<Path>, template_exts: &[impl AsRef<OsStr>]) -> bool {
        path.as_ref().extension().is_some_and(|ext| template_exts.iter().any(|x| x.as_ref() == ext))
    }

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

    pub fn set_compilation_result(&mut self, res: impl Display) {
        self.state = match replace(&mut self.state, AssetState::Raw) {
            AssetState::Template { src } => {
                AssetState::Compiled { src, res: res.to_string().into() }
            }
            other => other,
        };
    }
}

pub struct AssetManager {
    assets: Vec<Asset>,
    template_exts: Vec<OsStrView>,
    http_client: SyncClient,
}

impl Default for AssetManager {
    fn default() -> Self {
        Self {
            assets: default(),
            template_exts: vec!["html".into(), "css".into(), "svg".into()],
            http_client: default(),
        }
    }
}

impl AssetManager {
    pub fn assets(&self) -> impl Iterator<Item = &Asset> + Clone {
        self.assets.iter()
    }

    pub fn locate(&self, ptr: *const u8) -> Option<(StrView, Location)> {
        locate_in_multiple(
            ptr,
            self.assets.iter().map(|a| (a.path.clone(), a.src().unwrap_or_default())),
        )
    }

    pub fn register_template_ext(&mut self, ext: OsStrView) {
        self.template_exts.push(ext);
    }

    /// The function will return the same value until `set_compilation_result` is called
    pub fn next_uncompiled_asset(&self) -> Option<(StrView, Asset)> {
        self.assets
            .iter()
            .find_map(|asset| asset.template_src().map(|src| (src.clone(), asset.clone())))
    }

    pub fn save_compilation_result(&mut self, res: XmlNode) {
        if let Some(asset) = self.assets.iter_mut().find(|asset| asset.template_src().is_some()) {
            asset.set_compilation_result(res);
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
        is_template: Option<bool>,
    ) -> Result<&mut Asset> {
        if let Some(res) = self.assets.iter_mut().find(|asset| asset.path_or_url() == &path_or_url)
        {
            // SAFETY: the fn signature assets correctness, just circumventing borrow checker
            // errors
            return Ok(unsafe { assume_static_mut(res) });
        }

        let is_template =
            is_template.unwrap_or_else(|| Asset::is_template(&path_or_url, &self.template_exts));
        let ext;
        let src;
        (src, ext) = self.get_asset_metadata(&path_or_url, is_local, is_template)?;
        let state = src.map_or(AssetState::Raw, |src| AssetState::Template { src });
        let mut path = path_or_url;
        let mut url = None;
        if !is_local {
            url = Some(path);
            let n_remote_assets = self.assets.iter().filter(|asset| asset.url.is_some()).count();
            path = format!("cached/{n_remote_assets}.{ext}").into();
        }

        Ok(self.assets.push_mut(Asset { path, ext, url, state }))
    }

    pub async fn write_to_disk(&mut self, dst_root: impl AsRef<Path>) -> Result {
        let dst_root = dst_root.as_ref();
        let http_client = AsyncClient::new();

        let mut tasks = JoinSet::<Result>::new();
        for asset in self.assets.drain(..) {
            let dst = dst_root.join(&asset.path);

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
                            remove_file(&dst).await.with_context(|| {
                                format!("failed to remove {dst:?} to link it to {src:?}")
                            })?;
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
                        eprintln!("Wrote {} bytes to {dst:?}", res.len());
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
