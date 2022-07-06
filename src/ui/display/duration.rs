//! Utility to display durations

use clang_time_trace::Duration;
use std::io;

/// Display a duration in a human-readable format
pub fn display_duration(output: impl io::Write, duration: Duration) -> io::Result<()> {
    display_duration_impl(output, duration, None)
}

/// Implementation of display_duration that allows for HH:MM:SS format
fn display_duration_impl(
    mut output: impl io::Write,
    duration: Duration,
    hms: Option<HMS>,
) -> io::Result<()> {
    const MILLISECOND: Duration = 1000.0;
    const SECOND: Duration = 1000.0 * MILLISECOND;
    const MINUTE: Duration = 60.0 * SECOND;
    const HOUR: Duration = 60.0 * MINUTE;
    const DAY: Duration = 24.0 * HOUR;
    if duration >= DAY {
        let days = (duration / DAY).floor();
        write!(output, "{days}d ")?;
        display_duration_impl(output, duration - days * DAY, Some(HMS::ForceHour))
    } else if duration >= HOUR || hms == Some(HMS::ForceHour) {
        let hours = (duration / HOUR).floor();
        write!(output, "{hours}:")?;
        display_duration_impl(output, duration - hours * HOUR, Some(HMS::ForceMinute))
    } else if duration >= MINUTE || hms == Some(HMS::ForceMinute) {
        let minutes = (duration / MINUTE).floor();
        write!(output, "{minutes}:")?;
        display_duration_impl(output, duration - minutes * MINUTE, Some(HMS::ForceSecond))
    } else if duration >= SECOND || hms == Some(HMS::ForceSecond) {
        write!(output, "{:.2}", duration / SECOND)?;
        if hms != Some(HMS::ForceSecond) {
            write!(output, "s")?;
        }
        Ok(())
    } else if duration >= MILLISECOND {
        write!(output, "{:.2}ms", duration / MILLISECOND)
    } else {
        write!(output, "{duration}µs")
    }
}
//
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum HMS {
    ForceHour,
    ForceMinute,
    ForceSecond,
}
