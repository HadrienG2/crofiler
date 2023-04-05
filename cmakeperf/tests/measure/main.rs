//! Tests of the compilation profile measurement facility
//!
//! They are all ignored by default because you must run them on a reasonably
//! quiet system (i.e. not Github CI nodes) for them to succeed.

mod fixture;
mod log;

use fixture::{JobProperties, MeasurementTest};
use std::time::Duration;

#[test]
#[ignore = "Need a quied system"]
fn nothing() {
    MeasurementTest::new().start(true).finish()
}

#[test]
#[ignore = "Need a quied system"]
fn single() {
    MeasurementTest::new()
        .with_job(
            "hog 30M:1.0s 0M:0.2s",
            JobProperties {
                max_rss_mb: 30,
                wall_time: Duration::from_millis(1200),
                ..Default::default()
            },
        )
        .start(true)
        .finish()
}

#[test]
#[ignore = "Need a quied system"]
fn dual_seq() {
    MeasurementTest::new()
        .with_job(
            "hog 45M:1.5s 0M:0.2s",
            JobProperties {
                max_rss_mb: 45,
                wall_time: Duration::from_millis(1700),
                ..Default::default()
            },
        )
        .with_job(
            "hog 60M:2.0s 0M:0.2s",
            JobProperties {
                max_rss_mb: 60,
                wall_time: Duration::from_millis(2200),
                ..Default::default()
            },
        )
        .start(true)
        .finish()
}

#[test]
#[ignore = "Need a quied system"]
fn dual_par() {
    MeasurementTest::new()
        .with_job(
            "hog 75M:2.5s 0M:0.2s",
            JobProperties {
                max_rss_mb: 75,
                wall_time: Duration::from_millis(2700),
                ..Default::default()
            },
        )
        .with_job(
            "hog 90M:3.0s 0M:0.2s",
            JobProperties {
                max_rss_mb: 90,
                wall_time: Duration::from_millis(3200),
                ..Default::default()
            },
        )
        .start(false)
        .finish()
}

#[test]
#[ignore = "Need a quied system"]
fn kill() {
    MeasurementTest::new()
        .with_job(
            "hog 0M:30s",
            JobProperties {
                max_rss_mb: 0,
                wall_time: Duration::from_secs(30),
                ..Default::default()
            },
        )
        .start(true)
        .kill()
}

// TODO: Test with exit codes, stdout/stderr output, one and two levels of recursion
