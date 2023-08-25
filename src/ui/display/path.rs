//! File path handling

use super::DisplayConfig;
use clang_time_trace::InternedPath;
use std::{path::MAIN_SEPARATOR as PATH_SEPARATOR, sync::OnceLock};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

/// Render a possibly truncated version of a file path, that aims to fit within
/// a certain number of terminal columns
///
/// Unfortunately, in the age of Unicode, estimating the amount of terminal
/// columns that a certain string will cover when rendered is difficult. This
/// function uses the unicode-width crate's estimate, which itself assigns
/// width to code points using Unicode Standard Annex #11 then sums up across
/// all code points in the string.
///
/// This algorithm is not foolproof, for example it will mis-handle emoji
/// combinations like 👩 + 🔬 ➝ 👩‍🔬, as well as isolated combining characters
/// like this Korean jongseong: ᆨ. But in my testing, common Linux terminals
/// wouldn't handle those strings correctly either, likely because they use a
/// similar algorithm, so we're state of the art in this respect...
///
pub fn display_path(path: &InternedPath, config: DisplayConfig) -> Box<str> {
    match config {
        DisplayConfig::SingleLine { max_cols } => {
            truncate_path_iter(path.components().map(|c| c.value()), max_cols)
        }
        DisplayConfig::MultiLine { .. } => path.to_boxed_path().display().to_string().into(),
    }
}

/// Easily testable implementation of truncate_path that takes an iterator of
/// path components as input instead of an InternedPath
pub fn truncate_path_iter(
    mut components: impl Iterator<Item = impl AsRef<str>> + DoubleEndedIterator + Clone,
    cols: u16,
) -> Box<str> {
    // Track remaining column budget, keeping 1 spare column to insert an
    // ellipsis if needed
    assert!(cols > 0, "Need at least one column to display something");
    let cols = usize::from(cols);

    // Determine how many complete path components we can display
    let (accepted_front, accepted_back) = select_components(components.clone(), cols);

    // Can we at least display the file name ?
    let result = if accepted_back > 0 {
        // If so, accept the solution proposed by select_components
        display_components(components, (accepted_front, accepted_back))
    } else if cols >= MIN_FILENAME_WIDTH + 2 {
        // If not, and if the filename is long enough for this not to be
        // ridiculous, display a shortened file name where the middle is elided.
        display_filename(
            components.next_back().expect("Expected file name").as_ref(),
            cols,
        )
    } else {
        // Otherwise give up and emit a single ellipsis
        '…'.to_string().into()
    };

    // Sanity-check output, then emit it
    debug_assert!(
        result.width() <= cols,
        "Failed to honor requested column budget"
    );
    result
}

/// Minimal filename width (in terminal columns) for partial display
///
/// Above this limit, if truncate_path doesn't have enough terminal columns to
/// display a file name, it will abbreviate it as …/<begin>…<end>. Below this
/// limit, the file name will just be rendered as ….
const MIN_FILENAME_WIDTH: usize = 15;

/// Select path components at the front and the back of the path iterator in
/// order to produce a truncated path of the specified length
///
/// Return the number of selected components at the front and the back
fn select_components(
    mut components: impl Iterator<Item = impl AsRef<str>> + DoubleEndedIterator + Clone,
    mut cols: usize,
) -> (usize, usize) {
    // Check how many path components we can print on the front & back sides
    // We start from the back as that's where the filename lies, and that's the
    // single most important info we can display.
    let root_component = root_component();
    let num_components = components.clone().count();
    let mut accepted_front = 0;
    let mut accepted_back = 0;
    let mut front = false;
    let mut other_side_full = false;
    loop {
        // Check out next path component in desired direction
        let candidate = if front {
            components.next()
        } else {
            components.next_back()
        };

        // Assuming there is one, decide if it fits our column budget
        if let Some(candidate) = candidate {
            // The last processed path component is special
            let is_last_component = accepted_front + accepted_back == num_components - 1;

            // Check if we'll need a path separator. Normally, front path
            // components are followed by one, and back components are preceded
            // by one, but the fs root needs no separator and the last component
            // will use the separator of the following/preceding element.
            let candidate = candidate.as_ref();
            let need_separator = !(is_last_component || (candidate == root_component));
            let width_with_separator = candidate.width() + (need_separator as usize);

            // Until we reach the last path component, we must keep one terminal
            // column aside for a possible … elision ellipsis
            let available_cols = cols - (!is_last_component as usize);

            // If the component fits, accept it
            let fitting = width_with_separator <= available_cols;
            if fitting {
                if front {
                    accepted_front += 1;
                } else {
                    accepted_back += 1;
                }
                cols -= width_with_separator;
            }

            // As long as both sides fit, alternate between front and back
            front ^= !other_side_full;

            // Upon encountering a non-fitting candidate, stop looking in the
            // associated direction, and stop search once neither side fits.
            if !fitting {
                if other_side_full {
                    break;
                } else {
                    other_side_full = true;
                }
            }
        } else {
            // If there is no remaining path component, we're done
            break;
        }
    }
    (accepted_front, accepted_back)
}

/// Display the set of path components selected by select_components
fn display_components(
    mut components: impl Iterator<Item = impl AsRef<str>> + DoubleEndedIterator + Clone,
    (accepted_front, accepted_back): (usize, usize),
) -> Box<str> {
    // Set up storage
    let root_component = root_component();
    let num_components = components.clone().count();
    let mut buffer = String::new();

    // Add front components and associated separators
    for _ in 0..accepted_front {
        let component = components
            .next()
            .expect("There should be more components than accepted components");
        let component = component.as_ref();
        buffer.push_str(component);
        if component != root_component {
            buffer.push(PATH_SEPARATOR);
        }
    }

    // Handle possible component elision in the middle
    assert!(num_components >= accepted_front + accepted_back);
    let elided_components = num_components - (accepted_front + accepted_back);
    if elided_components > 0 {
        // Notify that path components were elided, then drop them
        buffer.push('…');
        for _ in 0..elided_components {
            components.next();
        }
    } else {
        // If no elision occurs, avoid double separator (front + back)
        buffer.pop();
    }

    // Add back components and associated separators
    for _ in 0..accepted_back {
        buffer.push(PATH_SEPARATOR);
        buffer.push_str(
            components
                .next()
                .expect("There should be more components than accepted components")
                .as_ref(),
        );
    }

    // Render path
    buffer.into()
}

/// Path component to be treated as the path root
fn root_component() -> &'static str {
    static ROOT_COMPONENT: OnceLock<Box<str>> = OnceLock::new();
    ROOT_COMPONENT
        .get_or_init(|| PATH_SEPARATOR.to_string().into())
        .as_ref()
}

/// Display only a file name, eliding some characters in the middle
fn display_filename(file_name: &str, mut cols: usize) -> Box<str> {
    // This function needs at least enough space to display the elided front
    // path and an ellipsis, and should not be called if there are enough
    // columns to display the file name unelided.
    debug_assert!(cols >= 3 && cols < file_name.width() + 2);

    // Start the path
    let mut buffer = String::new();
    buffer.push('…');
    buffer.push(PATH_SEPARATOR);
    cols -= buffer.width() + 1; // Account for future ellipsis

    // Add front graphemes
    let mut graphemes = file_name.graphemes(true);
    let max_front_cols = cols / 2;
    let mut rem_front_cols = max_front_cols;
    #[allow(clippy::while_let_on_iterator)]
    while let Some(front_grapheme) = graphemes.next() {
        if front_grapheme.width() <= rem_front_cols {
            buffer.push_str(front_grapheme);
            rem_front_cols -= front_grapheme.width();
        } else {
            break;
        }
    }
    cols -= max_front_cols - rem_front_cols;

    // Add middle ellipsis
    buffer.push('…');

    // Select back graphemes
    let mut back_graphemes = Vec::new();
    while let Some(back_grapheme) = graphemes.next_back() {
        if back_grapheme.width() <= cols {
            back_graphemes.push(back_grapheme);
            cols -= back_grapheme.width();
        } else {
            break;
        }
    }

    // Add back graphemes and emit result
    for back_grapheme in back_graphemes.into_iter().rev() {
        buffer.push_str(back_grapheme);
    }
    buffer.into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    fn path_components(path: &str) -> impl Iterator<Item = &str> + DoubleEndedIterator + Clone {
        Path::new(path).components().map(|c| {
            c.as_os_str()
                .to_str()
                .expect("Tested path components should be UTF-8")
        })
    }

    fn test_truncate_path(path: &str, cols: usize, expected: &str) {
        let actual = truncate_path_iter(path_components(path), cols as u16);
        assert_eq!(
            actual.as_ref(),
            expected,
            "Failed for path={path} and cols={cols}, expected {expected} but got {actual}"
        );
    }

    #[test]
    fn truncate_short_name() {
        assert!(MIN_FILENAME_WIDTH > "stuff.h".width());
        for cols in 1.."…/stuff.h".width() {
            test_truncate_path("/usr/include/stuff.h", cols, "…");
        }
        for cols in "…/stuff.h".width().."/…/stuff.h".width() {
            test_truncate_path("/usr/include/stuff.h", cols, "…/stuff.h");
        }
        for cols in "/…/stuff.h".width().."/usr/…/stuff.h".width() {
            test_truncate_path("/usr/include/stuff.h", cols, "/…/stuff.h");
        }
        for cols in "/usr/…/stuff.h".width().."/…/include/stuff.h".width() {
            test_truncate_path("/usr/include/stuff.h", cols, "/usr/…/stuff.h");
        }
        for cols in "/…/include/stuff.h".width().."/usr/include/stuff.h".width() {
            test_truncate_path("/usr/include/stuff.h", cols, "/…/include/stuff.h");
        }
        for cols in "/usr/include/stuff.h".width()..("/usr/include/stuff.h".width() + 2) {
            test_truncate_path("/usr/include/stuff.h", cols, "/usr/include/stuff.h");
        }
    }

    #[test]
    fn truncate_long_name() {
        const FILENAME: &'static str = "WowSuchALongHeaderName.hpp";
        assert!(MIN_FILENAME_WIDTH <= FILENAME.width());
        for cols in 1..MIN_FILENAME_WIDTH + 2 {
            test_truncate_path("/usr/include/WowSuchALongHeaderName.hpp", cols, "…");
        }
        for cols in (MIN_FILENAME_WIDTH + 2).."…/WowSuchALongHeaderName.hpp".width() {
            let expected = super::display_filename(FILENAME, cols);
            test_truncate_path(
                "/usr/include/WowSuchALongHeaderName.hpp",
                cols,
                expected.as_ref(),
            );
        }
        for cols in "…/WowSuchALongHeaderName.hpp".width().."/…/WowSuchALongHeaderName.hpp".width()
        {
            test_truncate_path(
                "/usr/include/WowSuchALongHeaderName.hpp",
                cols,
                "…/WowSuchALongHeaderName.hpp",
            );
        }
        for cols in
            "/…/WowSuchALongHeaderName.hpp".width().."/usr/…/WowSuchALongHeaderName.hpp".width()
        {
            test_truncate_path(
                "/usr/include/WowSuchALongHeaderName.hpp",
                cols,
                "/…/WowSuchALongHeaderName.hpp",
            );
        }
        for cols in "/usr/…/WowSuchALongHeaderName.hpp".width()
            .."/…/include/WowSuchALongHeaderName.hpp".width()
        {
            test_truncate_path(
                "/usr/include/WowSuchALongHeaderName.hpp",
                cols,
                "/usr/…/WowSuchALongHeaderName.hpp",
            );
        }
        for cols in "/…/include/WowSuchALongHeaderName.hpp".width()
            .."/usr/include/WowSuchALongHeaderName.hpp".width()
        {
            test_truncate_path(
                "/usr/include/WowSuchALongHeaderName.hpp",
                cols,
                "/…/include/WowSuchALongHeaderName.hpp",
            );
        }
        for cols in "/usr/include/WowSuchALongHeaderName.hpp".width()
            ..("/usr/include/WowSuchALongHeaderName.hpp".width() + 2)
        {
            test_truncate_path(
                "/usr/include/WowSuchALongHeaderName.hpp",
                cols,
                "/usr/include/WowSuchALongHeaderName.hpp",
            );
        }
    }

    #[test]
    fn select_components() {
        let test_select_components = |path, cols, expected| {
            let actual = super::select_components(path_components(path), cols);
            assert_eq!(
                actual, expected,
                "Failed for path={path} and cols={cols}, expected {expected:?} but got {actual:?}"
            );
        };
        for cols in 1.."/…".width() {
            test_select_components("/usr/include/stuff.h", cols, (0, 0));
        }
        for cols in "/…".width().."/usr/…".width() {
            test_select_components("/usr/include/stuff.h", cols, (1, 0));
        }
        for cols in "/usr/…".width().."…/stuff.h".width() {
            test_select_components("/usr/include/stuff.h", cols, (2, 0));
        }
        for cols in "…/stuff.h".width().."/…/stuff.h".width() {
            test_select_components("/usr/include/stuff.h", cols, (0, 1));
        }
        for cols in "/…/stuff.h".width().."/usr/…/stuff.h".width() {
            test_select_components("/usr/include/stuff.h", cols, (1, 1));
        }
        for cols in "/usr/…/stuff.h".width().."/…/include/stuff.h".width() {
            test_select_components("/usr/include/stuff.h", cols, (2, 1));
        }
        for cols in "/…/include/stuff.h".width().."/usr/include/stuff.h".width() {
            test_select_components("/usr/include/stuff.h", cols, (1, 2));
        }
        for cols in "/usr/include/stuff.h".width()..("/usr/include/stuff.h".width() + 2) {
            test_select_components("/usr/include/stuff.h", cols, (2, 2));
        }
    }

    #[test]
    fn display_components() {
        let test_display_component = |path, what, expected| {
            let actual = super::display_components(path_components(path), what);
            assert_eq!(
                actual.as_ref(),
                expected,
                "Failed for path={path} and selection {what:?}, expected {expected} but got {actual}"
            );
        };
        test_display_component("/usr/include/stuff.h", (0, 0), "…");
        test_display_component("/usr/include/stuff.h", (0, 1), "…/stuff.h");
        test_display_component("/usr/include/stuff.h", (0, 2), "…/include/stuff.h");
        test_display_component("/usr/include/stuff.h", (1, 0), "/…");
        test_display_component("/usr/include/stuff.h", (1, 1), "/…/stuff.h");
        test_display_component("/usr/include/stuff.h", (1, 2), "/…/include/stuff.h");
        test_display_component("/usr/include/stuff.h", (1, 3), "/usr/include/stuff.h");
        test_display_component("/usr/include/stuff.h", (2, 0), "/usr/…");
        test_display_component("/usr/include/stuff.h", (2, 1), "/usr/…/stuff.h");
        test_display_component("/usr/include/stuff.h", (2, 2), "/usr/include/stuff.h");
        test_display_component("/usr/include/stuff.h", (3, 0), "/usr/include/…");
        test_display_component("/usr/include/stuff.h", (3, 1), "/usr/include/stuff.h");
    }

    #[test]
    fn display_filename() {
        let test_display_filename = |file_name, cols, expected| {
            let actual = super::display_filename(file_name, cols);
            assert_eq!(
                actual.as_ref(), expected,
                "Failed for file name {file_name} and cols={cols}, expected {expected:?} but got {actual:?}"
            );
        };
        test_display_filename("stuff.h", 3, "…/…");
        test_display_filename("stuff.h", 4, "…/…h");
        test_display_filename("stuff.h", 5, "…/s…h");
        test_display_filename("stuff.h", 6, "…/s….h");
        test_display_filename("stuff.h", 7, "…/st….h");
        test_display_filename("stuff.h", 8, "…/st…f.h");
    }
}
