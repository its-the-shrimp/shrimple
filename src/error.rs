use crate::parser::Expected;
use anyhow::Error;
use shrimple_parser::{
    utils::{locate_in_multiple, FullLocation, WithSourceLine},
    FullParsingError,
};
use std::{
    fmt::{Debug, Display, Formatter, Write},
    path::Path,
    sync::Arc,
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
pub struct Expansions(pub Arc<[&'static str]>);

impl Display for Expansions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<not an error, but Expansions>")
    }
}

impl std::error::Error for Expansions {}

pub fn collect_template_expansion_info<Files, Filepath, Src>(error: Error, files: Files) -> Error
where
    Files: IntoIterator,
    Files::IntoIter: Iterator<Item = (Filepath, Src)> + Clone,
    Filepath: AsRef<Path>,
    Src: AsRef<str>,
{
    let root = error.root_cause();
    let loc = error.downcast_ref::<ExtraCtx<FullLocation<'static>>>().map(|x| x.0);
    let expansions = error.downcast_ref::<Expansions>().map(|x| &x.0);

    // the `\r` is prepended to discard the "Error: " that Rust prints before printing an error
    // returned from `main`
    let (mut msg, loc) = match root.downcast_ref::<FullParsingError<'static, Expected>>() {
        Some(err) => (
            err.reason.map_or_else(|| "\rthis is a bug :3".to_owned(), |x| format!("\rerror: {x}")),
            Some(err.loc),
        ),
        None => (format!("\rerror: {root}"), loc),
    };

    if let Some(loc) = loc {
        if write!(&mut msg, "\n--> {}", WithSourceLine(loc)).is_err() {
            return error;
        }
    }

    let files = files.into_iter();
    for &name in expansions.iter().flat_map(|x| x.iter().rev()) {
        if write!(&mut msg, "\n\n...while expanding template `<${name}>`").is_err() {
            return error;
        }
        if let Some((path, loc)) = locate_in_multiple(name.as_ptr(), files.clone()) {
            let loc = FullLocation { loc, path: path.as_ref() };
            if write!(&mut msg, "\n--> {}", WithSourceLine(loc)).is_err() {
                return error;
            }
        }
    }

    Error::msg(msg)
}
