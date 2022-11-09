//! Profiling dialogs and related functionality

pub mod display;
pub mod load;
pub mod measure;

use clang_time_trace::Duration;
use decorum::Finite;

/// Compute the percentage norm associated with a set of activities
fn percent_norm(total_duration: Duration) -> Finite<Duration> {
    Finite::<Duration>::from_inner(100.0 / total_duration)
}
