//! Full-build profiling facilities

pub mod cmakeperf;

use crate::build::{self, commands::CompilationDatabase};
use serde::Deserialize;
use std::{io, path::Path, time::Duration};
use thiserror::Error;

/// Default full-build profile location
pub const DEFAULT_LOCATION: &str = "./cmakeperf.csv";

/// Check if a build profile is up to date wrt the compilation database
///
/// Note that this is a necessary condition for profile freshness, but not a
/// sufficient one, because the compilation database does not know about all the
/// files that compilation commands depend on (e.g. it doesn't know headers)
///
pub fn is_up_to_date(
    profile_path: &Path,
    compilation_database: &CompilationDatabase,
) -> Result<bool, io::Error> {
    // Check profile mtime
    let profile_mtime = profile_path.metadata()?.modified()?;

    // Compare to compilation database mtime
    if profile_mtime
        < Path::new(build::commands::LOCATION)
            .metadata()?
            .modified()?
    {
        return Ok(false);
    }

    // Compare to mtime of every input file in compilation database
    for entry in compilation_database {
        if profile_mtime < entry.input().metadata()?.modified()? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Load a previously measured build profile
pub fn load(path: impl AsRef<Path>) -> Result<Profile, ProfileLoadError> {
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
#[derive(Debug, Deserialize)]
pub struct Unit {
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
impl Unit {
    /// Path to the file, starting from the build directory
    pub fn rel_path(&self) -> impl AsRef<Path> + '_ {
        &self.rel_path
    }

    /// Maximum observed memory usage in bytes
    pub fn max_rss_bytes(&self) -> usize {
        self.max_rss
    }

    /// Time taken to compile this file, if measured
    pub fn wall_time(&self) -> Option<Duration> {
        self.wall_time_secs.map(Duration::from_secs_f32)
    }
}

/// Full build profile
type Profile = Vec<Unit>;
