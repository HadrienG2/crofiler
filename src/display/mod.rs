//! Generic utilities to display things

use unicode_width::UnicodeWidthStr;

pub mod activity;
pub mod duration;
pub mod path;
pub mod stdio;

/// Truncate a string so that it only eats up n columns, by eating up the middle
pub fn truncate_string(input: &str, max_cols: u16) -> String {
    // Make sure the request makes sense, set up common infrastructure
    debug_assert!(input.width() > max_cols.into());
    debug_assert!(max_cols >= 1);
    let bytes = input.as_bytes();
    let mut result = String::new();
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
    result.push_str(last_good);
    result.push('…');

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
    result.push_str(last_good);
    result
}
