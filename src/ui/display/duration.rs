//! Utility to display durations

use clang_time_trace::{Duration, DAY, HOUR, MICROSECOND, MILLISECOND, MINUTE, SECOND};
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
    if duration >= 23.0 * HOUR + 59.0 * MINUTE + 59.995 * SECOND {
        let mut days = (duration / DAY).floor();
        let mut remainder = duration - days * DAY;
        if remainder >= 23.0 * HOUR + 59.0 * MINUTE + 59.995 * SECOND {
            days += 1.0;
            remainder = 0.0;
        }
        write!(output, "{days}d ")?;
        display_duration_impl(output, remainder, Some(HMS::ForceHour))
    } else if hms == Some(HMS::ForceHour) || duration >= 59.0 * MINUTE + 59.995 * SECOND {
        let mut hours = (duration / HOUR).floor();
        let mut remainder = duration - hours * HOUR;
        if remainder >= 59.0 * MINUTE + 59.995 * SECOND {
            hours += 1.0;
            remainder = 0.0;
        }
        if hms == Some(HMS::ForceHour) {
            write!(output, "{hours:02}:")?;
        } else {
            write!(output, "{hours}:")?;
        }
        display_duration_impl(output, remainder, Some(HMS::ForceMinute))
    } else if hms == Some(HMS::ForceMinute) || duration >= 59.995 * SECOND {
        let mut minutes = (duration / MINUTE).floor();
        let mut remainder = duration - minutes * MINUTE;
        if remainder >= 59.995 * SECOND {
            minutes += 1.0;
            remainder = 0.0;
        }
        if hms == Some(HMS::ForceMinute) {
            write!(output, "{minutes:02}:")?;
        } else {
            write!(output, "{minutes}:")?;
        }
        display_duration_impl(output, remainder, Some(HMS::ForceSecond))
    } else if duration >= 0.999995 * SECOND || hms == Some(HMS::ForceSecond) {
        if hms == Some(HMS::ForceSecond) {
            write!(output, "{:05.2}", duration / SECOND)?;
        } else {
            write!(output, "{:.2}s", duration / SECOND)?;
        }
        Ok(())
    } else if duration >= MILLISECOND {
        write!(output, "{:.2}ms", duration / MILLISECOND)
    } else {
        write!(output, "{}??s", duration / MICROSECOND)
    }
}
//
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum HMS {
    ForceHour,
    ForceMinute,
    ForceSecond,
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    #[test]
    fn display_duration() {
        let mut buffer = Vec::new();
        let mut check_display = |duration, expected: &str| {
            buffer.clear();
            assert_matches!(super::display_duration(&mut buffer, duration), Ok(()));
            assert_eq!(
                buffer,
                expected.as_bytes(),
                "Expected {}, got {:?}",
                expected,
                std::str::from_utf8(&buffer)
            );
        };
        check_display(4.12345 * MICROSECOND, "4.12345??s");
        check_display(999.999 * MICROSECOND, "999.999??s");

        check_display(1.0 * MILLISECOND, "1.00ms");
        check_display(1.234 * MILLISECOND, "1.23ms");
        check_display(999.994 * MILLISECOND, "999.99ms");

        check_display(999.995 * MILLISECOND, "1.00s");
        check_display(1.6 * SECOND, "1.60s");
        check_display(4.321 * SECOND, "4.32s");
        check_display(59.994 * SECOND, "59.99s");

        check_display(59.995 * SECOND, "1:00.00");
        check_display(3.0 * MINUTE, "3:00.00");
        check_display(5.0 * MINUTE + 6.789 * SECOND, "5:06.79");
        check_display(59.0 * MINUTE + 59.994 * SECOND, "59:59.99");

        check_display(59.0 * MINUTE + 59.995 * SECOND, "1:00:00.00");
        check_display(2.0 * HOUR + 3.0 * MINUTE + 7.654 * SECOND, "2:03:07.65");
        check_display(23.0 * HOUR + 59.0 * MINUTE + 59.994 * SECOND, "23:59:59.99");

        check_display(
            23.0 * HOUR + 59.0 * MINUTE + 59.995 * SECOND,
            "1d 00:00:00.00",
        );
        check_display(
            8.0 * DAY + 6.0 * HOUR + 2.0 * MINUTE + 1.357 * SECOND,
            "8d 06:02:01.36",
        );
    }
}
