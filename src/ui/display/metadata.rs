//! Display for top-level trace metadata

use chrono::{SecondsFormat, TimeZone};
use clang_time_trace::{ClangTrace, MICROSECOND, SECOND};
use unicode_width::UnicodeWidthStr;

/// Display a trace's metadata, honoring a certain column budget but using up
/// multiple lines if needed.
pub fn metadata(trace: &ClangTrace, max_cols: u16) -> String {
    // Must keep one column aside to print out trailing comma/dot
    let effective_max_cols = max_cols as usize - 1;
    let mut buf = String::new();

    // Display process name
    const PROCESS_NAME_HEADER: &str = "Data from ";
    if PROCESS_NAME_HEADER.width() < effective_max_cols {
        buf.push_str(PROCESS_NAME_HEADER);
        let buf_width = buf.width();
        let mut buf_bytes = buf.into_bytes();
        super::display_string(
            &mut buf_bytes,
            trace.process_name(),
            (effective_max_cols - buf_width) as u16,
        )
        .expect("Writing to a buffer shouldn't fail");
        buf = String::from_utf8(buf_bytes).expect("display_string should produce UTF-8 data");
    } else {
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

// FIXME: Add tests
