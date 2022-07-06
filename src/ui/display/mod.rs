//! Generic utilities to display things

use std::io;
use unicode_width::UnicodeWidthStr;

pub mod activity;
pub mod duration;
pub mod metadata;
pub mod path;

/// Truncate a string so that it only eats up n columns, by eating up the middle
pub fn display_string(mut output: impl io::Write, input: &str, max_cols: u16) -> io::Result<()> {
    // Handle trivial case
    if input.width() <= max_cols.into() {
        return write!(output, "{input}");
    }

    // Make sure the request makes sense, set up common infrastructure
    debug_assert!(max_cols >= 1);
    let bytes = input.as_bytes();
    let mut last_good = "";

    // Split our column budget into a header and trailer
    let trailer_cols = (max_cols - 1) / 2;
    let header_cols = max_cols - 1 - trailer_cols;

    // Find a terminal header with the right number of columns
    let mut header_bytes = header_cols;
    loop {
        let header_candidate = std::str::from_utf8(&bytes[..header_bytes.into()]);
        if let Ok(candidate) = header_candidate {
            if candidate.width() > header_cols.into() {
                break;
            } else {
                last_good = candidate;
            }
        }
        header_bytes += 1;
    }

    // Start printing out the result accordingly
    write!(output, "{last_good}…")?;

    // Find a terminal trailer with the right amount of columns
    let mut trailer_start = bytes.len() - usize::from(trailer_cols);
    loop {
        let trailer_candidate = std::str::from_utf8(&bytes[trailer_start..]);
        if let Ok(candidate) = trailer_candidate {
            if candidate.width() > trailer_cols.into() {
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
