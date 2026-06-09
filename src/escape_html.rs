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

        for chunk in self.0.as_ref().split_inclusive(['&', '<', '>']) {
            let mut chars = chunk.chars();
            let (normal, escaped) = match chars.next_back() {
                Some('&') => (chars.as_str(), "&amp;"),
                Some('<') => (chars.as_str(), "&lt;"),
                Some('>') => (chars.as_str(), "&gt;"),
                _ => (chunk, ""),
            };
            f.write_str(normal)?;
            f.write_str(escaped)?;
        }

        Ok(())
    }
}

#[test]
fn no_unicode_distortion() {
    let example = "из Łódź в 大阪";

    assert_eq!(
        example,
        &format!("{:#}", EscapeHtml(example)),
        "`EscapeHtml` distorted Unicode"
    );
}
