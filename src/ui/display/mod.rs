//! Generic utilities to display things

use std::io;
use unicode_width::UnicodeWidthStr;

pub mod activity;
pub mod duration;
pub mod metadata;
pub mod path;

/// Truncate a string so that it only eats up n columns, by eating up the middle
/// Assumes absence of line feeds in the input string.
pub fn display_string(mut output: impl io::Write, input: &str, max_cols: u16) -> io::Result<()> {
    // Handle trivial case
    if input.width() <= max_cols.into() {
        return write!(output, "{input}");
    }
    debug_assert!(input.chars().all(|c| c != '\r' && c != '\n'));

    // Make sure the request makes sense, set up common infrastructure
    assert!(max_cols >= 1);
    let bytes = input.as_bytes();
    let mut last_good = "";

    // Split our column budget into a header and trailer
    let max_header_cols = (max_cols - 1) / 2;
    let mut header_cols = 0;

    // Find a terminal header with the right number of columns
    let mut header_bytes = header_cols;
    loop {
        let header_candidate = std::str::from_utf8(&bytes[..header_bytes.into()]);
        if let Ok(candidate) = header_candidate {
            if candidate.width() > max_header_cols.into() {
                break;
            } else {
                header_cols = candidate.width() as u16;
                last_good = candidate;
            }
        }
        header_bytes += 1;
    }

    // Start printing out the result accordingly
    write!(output, "{last_good}…")?;

    // Find a terminal trailer with the right amount of columns
    let max_trailer_cols = max_cols - 1 - header_cols;
    let mut trailer_start = bytes.len() - usize::from(max_trailer_cols);
    loop {
        let trailer_candidate = std::str::from_utf8(&bytes[trailer_start..]);
        if let Ok(candidate) = trailer_candidate {
            if candidate.width() > max_trailer_cols.into() {
                break;
            } else {
                last_good = candidate;
            }
        }
        trailer_start -= 1;
    }

    // Emit the result
    write!(output, "{last_good}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    #[test]
    fn display_string() {
        let mut display = Vec::new();
        const MOCK_STR: &str = "Coucou, tu veux voir mon string ?";
        let mut check_display = |max_cols, expected_display: &str| {
            display.clear();
            assert_matches!(
                super::display_string(&mut display, MOCK_STR, max_cols),
                Ok(())
            );
            assert_eq!(
                display,
                expected_display.as_bytes(),
                "Expected display {:?} (width = {}), got display {:?} (width = {:?})",
                expected_display,
                expected_display.width(),
                std::str::from_utf8(&display),
                std::str::from_utf8(&display).map(|s| s.width())
            );
        };
        check_display(33, MOCK_STR);
        check_display(32, "Coucou, tu veux…oir mon string ?");
        check_display(31, "Coucou, tu veux…ir mon string ?");
        check_display(29, "Coucou, tu veu…r mon string ?");
        check_display(3, "C…?");
        check_display(1, "…");
    }
}
