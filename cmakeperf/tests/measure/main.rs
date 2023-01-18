//! Tests of the compilation profile measurement facility

mod fixture;
mod log;

use fixture::{JobProperties, MeasurementTest};
use std::time::Duration;

#[test]
fn nothing() {
    MeasurementTest::new().start(true).finish()
}

#[test]
fn single() {
    MeasurementTest::new()
        .with_job(
            "hog 30M:0.5s 0M:0.1s",
            JobProperties {
                max_rss_mb: 30,
                wall_time: Duration::from_millis(500),
                ..Default::default()
            },
        )
        .start(true)
        .finish()
}

#[test]
fn dual_seq() {
    MeasurementTest::new()
        .with_job(
            "hog 45M:0.75s 0M:0.1s",
            JobProperties {
                max_rss_mb: 45,
                wall_time: Duration::from_millis(850),
                ..Default::default()
            },
        )
        .with_job(
            "hog 60M:1.0s 0M:0.1s",
            JobProperties {
                max_rss_mb: 60,
                wall_time: Duration::from_millis(1100),
                ..Default::default()
            },
        )
        .start(true)
        .finish()
}

#[test]
fn dual_par() {
    MeasurementTest::new()
        .with_job(
            "hog 75M:1.25s 0M:0.1s",
            JobProperties {
                max_rss_mb: 75,
                wall_time: Duration::from_millis(1350),
                ..Default::default()
            },
        )
        .with_job(
            "hog 90M:1.5s 0M:0.1s",
            JobProperties {
                max_rss_mb: 90,
                wall_time: Duration::from_millis(1600),
                ..Default::default()
            },
        )
        .start(false)
        .finish()
}

#[test]
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
