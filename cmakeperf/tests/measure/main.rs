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
        .add_job(
            "hog 1M:0.5s 10M:0.1s 1M:0.1s",
            JobProperties {
                max_rss_mb: 10,
                wall_time: Duration::from_millis(700),
                ..Default::default()
            },
        )
        .start(true)
        .finish()
}

#[test]
fn dual_seq() {
    MeasurementTest::new()
        .add_job(
            "hog 1M:0.5s 21M:0.1s 1M:0.15s",
            JobProperties {
                max_rss_mb: 21,
                wall_time: Duration::from_millis(750),
                ..Default::default()
            },
        )
        .add_job(
            "hog 1M:0.5s 32M:0.1s 1M:0.2s",
            JobProperties {
                max_rss_mb: 32,
                wall_time: Duration::from_millis(800),
                ..Default::default()
            },
        )
        .start(true)
        .finish()
}

#[test]
fn dual_par() {
    MeasurementTest::new()
        .add_job(
            "hog 30M:0.5s 43M:0.1s 1M:0.25s",
            JobProperties {
                max_rss_mb: 43,
                wall_time: Duration::from_millis(850),
                ..Default::default()
            },
        )
        .add_job(
            "hog 30M:0.5s 54M:0.1s 1M:0.3s",
            JobProperties {
                max_rss_mb: 54,
                wall_time: Duration::from_millis(900),
                ..Default::default()
            },
        )
        .start(false)
        .finish()
}

#[test]
fn kill() {
    MeasurementTest::new()
        .add_job(
            "hog 1M:30s",
            JobProperties {
                max_rss_mb: 1,
                wall_time: Duration::from_secs(30),
                ..Default::default()
            },
        )
        .start(true)
        .kill()
}

// TODO: Test with exit codes, stdout/stderr output, one and two levels of recursion
