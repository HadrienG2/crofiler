//! Display for top-level trace metadata

use chrono::{SecondsFormat, TimeZone};
use clang_time_trace::{ClangTrace, MICROSECOND, SECOND};
use unicode_width::UnicodeWidthStr;

/// Display a trace's metadata, honoring a certain column budget but using up
/// multiple lines if needed.
pub fn metadata(trace: &ClangTrace, max_cols: u16) -> String {
    // Must keep one column aside to print out trailing comma/dot
    assert!(max_cols >= 1);
    let effective_max_cols = max_cols as usize - 1;

    // Display process name
    let mut buf = "Data from ".to_owned();
    buf.push_str(trace.process_name());
    if buf.width() > effective_max_cols {
        buf.clear();
        buf.push('…');
        return buf;
    }

    // Display extra metadata, with a line feed if need be
    let mut last_line_width = buf.width();
    let mut display_more = |buf: &mut String, mut extra: String| {
        // Abbreviate what doesn't fit
        if extra.width() > effective_max_cols {
            extra = "…".to_owned();
        }

        // Add a line feed if needed
        buf.push(',');
        last_line_width += 1;
        if last_line_width + 1 + extra.width() > effective_max_cols {
            buf.push('\n');
            last_line_width = 0;
        } else {
            buf.push(' ');
            last_line_width += 1;
        }
        buf.push_str(&extra);
        last_line_width += extra.width();
    };
    if let Some(pid) = trace.pid() {
        display_more(&mut buf, format!("pid {pid}"));
    }
    if let Some(name) = trace.thread_name() {
        display_more(&mut buf, format!("thread {name:?}"));
    }
    if let Some(bot) = trace.beginning_of_time() {
        let mut display = "recorded ".to_owned();
        let seconds = (bot / SECOND).floor();
        let nanoseconds = (bot - seconds * SECOND) / MICROSECOND * 1000.0;
        let bot = chrono::Utc
            .timestamp(seconds as _, nanoseconds as _)
            .to_rfc3339_opts(SecondsFormat::AutoSi, true);
        display.push_str(&bot);
        display_more(&mut buf, display);
    }
    buf.push('.');
    buf
}

#[cfg(test)]
mod tests {
    use crate::tests::TEST_TRACE;

    #[test]
    fn metadata() {
        let trace = TEST_TRACE.lock().unwrap();
        assert_eq!(super::metadata(&trace, 89), "Data from clang-14.0.5, pid 5884, thread \"clang++\", recorded 2022-06-29T13:18:22.311015Z.");
        assert_eq!(super::metadata(&trace, 88), "Data from clang-14.0.5, pid 5884, thread \"clang++\",\nrecorded 2022-06-29T13:18:22.311015Z.");
        assert_eq!(super::metadata(&trace, 51), "Data from clang-14.0.5, pid 5884, thread \"clang++\",\nrecorded 2022-06-29T13:18:22.311015Z.");
        assert_eq!(super::metadata(&trace, 50), "Data from clang-14.0.5, pid 5884,\nthread \"clang++\",\nrecorded 2022-06-29T13:18:22.311015Z.");
        assert_eq!(super::metadata(&trace, 37), "Data from clang-14.0.5, pid 5884,\nthread \"clang++\",\nrecorded 2022-06-29T13:18:22.311015Z.");
        assert_eq!(
            super::metadata(&trace, 36),
            "Data from clang-14.0.5, pid 5884,\nthread \"clang++\", …."
        );
        assert_eq!(
            super::metadata(&trace, 33),
            "Data from clang-14.0.5, pid 5884,\nthread \"clang++\", …."
        );
        assert_eq!(
            super::metadata(&trace, 32),
            "Data from clang-14.0.5,\npid 5884, thread \"clang++\", …."
        );
        assert_eq!(
            super::metadata(&trace, 30),
            "Data from clang-14.0.5,\npid 5884, thread \"clang++\", …."
        );
        assert_eq!(
            super::metadata(&trace, 29),
            "Data from clang-14.0.5,\npid 5884, thread \"clang++\",\n…."
        );
        assert_eq!(
            super::metadata(&trace, 27),
            "Data from clang-14.0.5,\npid 5884, thread \"clang++\",\n…."
        );
        assert_eq!(
            super::metadata(&trace, 26),
            "Data from clang-14.0.5,\npid 5884,\nthread \"clang++\", …."
        );
        assert_eq!(
            super::metadata(&trace, 23),
            "Data from clang-14.0.5,\npid 5884,\nthread \"clang++\", …."
        );
        assert_eq!(super::metadata(&trace, 22), "…");
    }
}
