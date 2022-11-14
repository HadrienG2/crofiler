//! Full-build profiling facilities

use serde::Deserialize;
use std::{io, path::Path, time::Duration};
use thiserror::Error;

/// Default full-build profile location
pub const DEFAULT_LOCATION: &str = "./cmakeperf.csv";

/// Load a previously measured build profile
pub fn load(path: impl AsRef<Path>) -> Result<BuildProfile, ProfileLoadError> {
    let mut reader = match csv::Reader::from_path(path.as_ref()) {
        Err(e) => match e.kind() {
            csv::ErrorKind::Io(e) if e.kind() == io::ErrorKind::NotFound => {
                return Err(ProfileLoadError::FileNotFound)
            }
            _other => return Err(ProfileLoadError::ParseError(e)),
        },
        other => other?,
    };

    let mut result = Vec::new();
    for unit in reader.deserialize() {
        result.push(unit?);
    }
    Ok(result)
}

/// Failure to load a build profile from a file
#[derive(Debug, Error)]
pub enum ProfileLoadError {
    /// Build profile file not found
    #[error("build profile not found")]
    FileNotFound,

    /// Failed to process the build profile for another reason
    #[error("failed to process build profile ({0})")]
    ParseError(#[from] csv::Error),
}

/// Record from the build profile CSV file
///
/// This summarizes the compilation performance of one compilation unit.
///
#[derive(Clone, Debug, Deserialize)]
pub struct UnitProfile {
    /// Relative path to the source file, starting from the build directory
    #[serde(rename = "file")]
    rel_path: Box<Path>,

    /// Maximum observed RSS memory usage during compilation, in bytes
    max_rss: usize,

    /// Wall-clock time taken to compile this file, if measured
    #[serde(rename = "time")]
    wall_time_secs: Option<f32>,
}
//
impl UnitProfile {
    /// Path to the file, starting from the build directory
    pub fn rel_path(&self) -> &Path {
        &self.rel_path
    }

    /// Maximum observed memory usage in bytes
    pub fn max_rss_bytes(&self) -> usize {
        self.max_rss
    }

    /// Time taken to compile this file, if measured
    ///
    /// If the compilation profile contains invalid duration values, these will
    /// be reported as an "error" containing the raw floating-point number.
    ///
    pub fn wall_time(&self) -> Option<Result<Duration, f32>> {
        let secs_f32 = self.wall_time_secs?;
        if !secs_f32.is_finite() {
            return Some(Err(secs_f32));
        }
        if secs_f32 < 0.0 {
            return Some(Err(secs_f32));
        }
        if secs_f32 > u64::MAX as f32 + 0.999999999 {
            return Some(Err(secs_f32));
        }
        Some(Ok(Duration::from_secs(secs_f32 as u64).saturating_add(
            Duration::from_nanos((secs_f32.fract() * 1_000_000_000.0) as u64),
        )))
    }
}

/// Full build profile
pub type BuildProfile = Vec<UnitProfile>;

// FIXME: Add unit tests
