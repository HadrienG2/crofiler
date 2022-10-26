//! Generic utilities to display things

use std::io;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

pub mod activity;
pub mod duration;
pub mod metadata;
pub mod path;

/// Text display configuration
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum DisplayConfig {
    /// Output text should fit on the current line (no line feed, no overflow)
    SingleLine {
        /// Column budget for this display
        max_cols: u16,
    },

    /// Output text is allowed to use multiple lines, either through the TUI's
    /// automatic word wrapping or pretty-printing.
    ///
    /// Going above the column budget is preferred over inserting semantically
    /// nonsensical line breaks. Either word wrapping or horizontal scrolling
    /// will be enabled when that happens.
    ///
    MultiLine {
        /// Total number of terminal columns available across the entire display
        tot_cols: u16,

        /// Number of terminal columns that were taken up on the first line
        /// before this multiline display started
        header_cols: u16,

        /// Number of terminal columns that will be taken up on the last line
        /// after this multiline display ends
        trailer_cols: u16,
    },
}

/// Truncate a string so that it only eats up n columns, by eating up the middle
/// Assumes absence of line feeds in the input string.
pub fn display_string(
    mut output: impl io::Write,
    input: &str,
    config: DisplayConfig,
) -> io::Result<()> {
    // Handle trivial cases
    let max_cols = match config {
        DisplayConfig::SingleLine { max_cols } => max_cols,
        DisplayConfig::MultiLine { .. } => return write!(output, "{input}"),
    } as usize;
    if input.width() <= max_cols {
        return write!(output, "{input}");
    }

    // Make sure the request makes sense
    debug_assert!(input.chars().all(|c| c != '\r' && c != '\n'));
    assert!(max_cols >= 1);

    // Split our column budget into a header and trailer
    let max_header_cols = (max_cols - 1) / 2;
    let mut header_cols = 0;

    // Find a terminal header with the right number of columns
    let mut header_end = 0;
    for (offset, grapheme) in input.grapheme_indices(true) {
        let new_header_cols = header_cols + grapheme.width();
        if new_header_cols <= max_header_cols {
            header_cols = new_header_cols;
            header_end = offset + grapheme.len();
        } else {
            break;
        }
    }

    // Start printing out the result accordingly
    write!(output, "{}…", &input[..header_end])?;

    // Find a terminal trailer with the right amount of columns
    let max_trailer_cols = max_cols - 1 - header_cols;
    let mut trailer_cols = 0;
    let mut trailer_start = input.len();
    for (offset, grapheme) in input.grapheme_indices(true).rev() {
        let new_trailer_cols = trailer_cols + grapheme.width();
        if new_trailer_cols <= max_trailer_cols {
            trailer_cols = new_trailer_cols;
            trailer_start = offset;
        } else {
            break;
        }
    }

    // Emit the result
    write!(output, "{}", &input[trailer_start..])
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
                super::display_string(
                    &mut display,
                    MOCK_STR,
                    DisplayConfig::SingleLine { max_cols }
                ),
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
            display.clear();
            assert_matches!(
                super::display_string(
                    &mut display,
                    MOCK_STR,
                    DisplayConfig::MultiLine {
                        tot_cols: max_cols,
                        header_cols: 1,
                        trailer_cols: 2
                    }
                ),
                Ok(())
            );
            assert_eq!(
                display,
                MOCK_STR.as_bytes(),
                "Expected multiline display {:?} (width = {}), got display {:?} (width = {:?})",
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
