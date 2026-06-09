use {
    crate::{asset::Asset, lexer::Expected, view::StrView},
    shrimple_parser::{
        FullParsingError,
        utils::{FullLocation, PathLike, WithSourceLine, locate_in_multiple},
    },
    std::{
        backtrace::BacktraceStatus,
        fmt::{Debug, Display, Formatter, Write},
        sync::Arc,
    },
};

#[derive(Debug, Clone, Copy)]
pub struct ExtraCtx<T>(pub T);

impl<T> Display for ExtraCtx<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("<not an error>")
    }
}

impl<T: Debug> std::error::Error for ExtraCtx<T> {}

/// The contained strings are the templates' names in the source code at the location where it's
/// used.
#[derive(Debug, Clone)]
pub struct Expansions(pub Arc<[StrView]>);

impl Display for Expansions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<not an error, but Expansions>")
    }
}

impl std::error::Error for Expansions {}

pub fn collect_template_expansion_info<'asset>(
    error: &anyhow::Error,
    assets: impl IntoIterator<Item = &'asset Asset, IntoIter: Clone>,
) -> anyhow::Error {
    let root = error.root_cause();

    // the `\r` is prepended to discard the "Error: " that Rust prints before printing an error
    // returned from `main`
    let (mut msg, loc) = match root.downcast_ref::<FullParsingError<'static, Expected>>() {
        Some(err) => (
            err.reason.map_or_else(|| "\rthis is a bug :3".to_owned(), |x| format!("\rerror: {x}")),
            Some(&err.loc),
        ),
        None => (
            format!("\rerror: {root}"),
            error.downcast_ref::<ExtraCtx<FullLocation<'static>>>().map(|x| &x.0),
        ),
    };

    if let Some(loc) = loc {
        _ = write!(&mut msg, "\n--> {}", WithSourceLine(loc));
    }

    let assets = assets.into_iter();
    let expansions = error.downcast_ref::<Expansions>().map(|x| &x.0);
    for name in expansions.iter().flat_map(|x| x.iter().rev()) {
        _ = write!(&mut msg, "\n\n...while expanding template `<${name}>`");
        if let Some((path, loc)) = locate_in_multiple(
            name.as_ptr(),
            assets.clone().filter_map(|asset| Some((&asset.path, asset.src()?))),
        ) {
            let loc = FullLocation { loc, path: path.into_path_bytes() };
            _ = write!(&mut msg, "\n--> {}", WithSourceLine(&loc));
        }
    }

    let backtrace = error.backtrace();
    if backtrace.status() == BacktraceStatus::Captured {
        _ = write!(&mut msg, "\nOriginal backtrace:\n{backtrace}");
    }

    anyhow::Error::msg(msg)
}
