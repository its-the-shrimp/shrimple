use std::fmt::{Debug, Display, Formatter, Write};

/// Prints `T`, which must provide an str reference, with `<`, `>` & `&` escaped as in HTML,
/// if the struct is to be printed in an [alternate](Formatter::alternate) way
pub struct EscapeHtml<T>(pub T);

impl<S: AsRef<str>> Debug for EscapeHtml<S> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_char('"')?;
        Display::fmt(&self, f)?;
        f.write_char('"')?;
        Ok(())
    }
}

impl<S: AsRef<str>> Display for EscapeHtml<S> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if !f.alternate() {
            return f.write_str(self.0.as_ref());
        }

        for byte in self.0.as_ref().bytes() {
            match byte {
                b'&' => f.write_str("&amp;")?,
                b'<' => f.write_str("&lt;")?,
                b'>' => f.write_str("&gt;")?,
                // Safety: we're writing them all anyway, not breaking any UTF-8 sequences.
                _ => unsafe {
                    f.write_char(char::from_u32_unchecked(byte.into()))?;
                },
            }
        }
        Ok(())
    }
}
