use std::{error::Error, fmt::{Display, Formatter, self, Write, Debug}, path::Path};
use nom::error::{Error as NomError, ErrorKind as NomErrorKind};

pub struct ErrorWithSource {
    filename: Box<Path>,
    line: usize,
    column: usize,
    source: Box<str>,
    #[cfg(debug_assertions)]
    code: NomErrorKind,
}

impl Display for ErrorWithSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let &Self {
            ref filename,
            line,
            column,
            ref source,
            #[cfg(debug_assertions)] code: kind,
        } = self;
        writeln!(f, "{}:\n{} | {}", filename.display(), line, source)?;
        for _ in 0 .. (line as f32).log10().ceil() as usize + 2 + column {
            f.write_char(' ')?
        }
        f.write_str("^\n")?;
        #[cfg(debug_assertions)]
        writeln!(f, "error kind: {kind:?}")?;
        Ok(())
    }
}

impl Debug for ErrorWithSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Error for ErrorWithSource {}

impl ErrorWithSource {
    /// Detects the location of the start of `error.input` in `contents` using pointer arithmetic.
    /// If `substr` is before `contents` in memory,
    /// the error will point at the first character of `contents`.
    /// If `substr` is after `contents` in memory,
    /// the error will point at the last character of `contents`.
    /// Such behaviour might seem stupid, but what would u do otherwise?
    /// Panic?
    /// Return an error while returning an error?
    /// Cmon.
    pub fn from_nom_error(filename: &Path, contents: &str, error: NomError<&str>) -> Self {
        if contents.is_empty() {
            return Self {
                filename: filename.into(),
                line: 1,
                column: 0,
                source: "".into(),
                #[cfg(debug_assertions)]
                code: error.code,
            }
        }
        let offset = (error.input.as_ptr() as usize)
            .checked_sub(contents.as_ptr() as usize)
            .map_or(0, |x| x.min(contents.len() - 1));
        let (mut line, mut column, mut this_line_start, mut source) = (1, 0, 0, None);
        for (at, ch) in contents.char_indices() {
            match (ch, at > offset) {
                ('\n', true) => {
                    // Safety:
                    // `at` is guaranteed to be a valid offset into `contents` by the iterator;
                    // `this_line_start` is always in the bounds `0 ..= at` in this branch.
                    source = Some(unsafe { contents.get_unchecked(this_line_start .. at) });
                    break
                }
                ('\n', false) => {
                    this_line_start = at + 1;
                    line += 1;
                    column = 0
                }
                (_, false) => column += 1,
                _ => (),
            }
        }
        Self {
            filename: filename.into(),
            line,
            column,
            #[cfg(debug_assertions)]
            code: error.code,
            source: source
                .unwrap_or_else(|| unsafe { contents.get_unchecked(this_line_start ..) })
                .into(),
        }
    }
}
