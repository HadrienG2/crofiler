//! Full-build profiling facilities

use serde::{Deserialize, Serialize};
use std::{
    io::{self, Read},
    path::Path,
    time::Duration,
};
use thiserror::Error;

/// Default full-build profile location
pub const DEFAULT_LOCATION: &str = "./cmakeperf.csv";

/// Load a previously measured build profile
pub fn load(path: impl AsRef<Path>) -> Result<BuildProfile, ProfileLoadError> {
    let reader = match csv::Reader::from_path(path.as_ref()) {
        Err(e) => match e.kind() {
            csv::ErrorKind::Io(e) if e.kind() == io::ErrorKind::NotFound => {
                return Err(ProfileLoadError::FileNotFound)
            }
            _other => return Err(ProfileLoadError::ParseError(e)),
        },
        other => other?,
    };
    from_csv_reader(reader)
}

/// Like `load()`, but takes any CSV reader, not necessarily a file reader
fn from_csv_reader<R: Read>(mut reader: csv::Reader<R>) -> Result<BuildProfile, ProfileLoadError> {
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
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct UnitProfile {
    /// Relative path to the source file, starting from the build directory
    #[serde(rename = "file")]
    rel_path: Box<Path>,

    /// Maximum observed RSS memory usage during compilation, in bytes
    max_rss: u64,

    /// Wall-clock time taken to compile this file, if measured
    #[serde(rename = "time")]
    wall_time_secs: Option<f32>,
}
//
impl UnitProfile {
    /// Create a new unit profile
    pub fn new(
        rel_path: impl Into<Box<Path>>,
        max_rss_bytes: u64,
        wall_time: Option<Duration>,
    ) -> Self {
        Self {
            rel_path: rel_path.into(),
            max_rss: max_rss_bytes,
            wall_time_secs: wall_time.map(|time| time.as_secs_f32()),
        }
    }

    /// Path to the file, starting from the build directory
    pub fn rel_path(&self) -> &Path {
        &self.rel_path
    }

    /// Maximum observed memory usage in bytes
    pub fn max_rss_bytes(&self) -> u64 {
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
        if secs_f32 > u64::MAX as f32 + 0.999_999_999 {
            return Some(Err(secs_f32));
        }
        Some(Ok(Duration::from_secs(secs_f32 as u64).saturating_add(
            Duration::from_nanos((secs_f32.fract() * 1_000_000_000.0) as u64),
        )))
    }
}

/// Full build profile
pub type BuildProfile = Vec<UnitProfile>;

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;
    use std::path::PathBuf;

    // Unit profile construction from Rust code
    #[quickcheck]
    fn unit_profile_new(rel_path: PathBuf, max_rss_bytes: u64, wall_time: Option<Duration>) {
        let unit_profile = UnitProfile::new(rel_path.clone(), max_rss_bytes, wall_time);
        assert_eq!(unit_profile.rel_path(), rel_path);
        assert_eq!(unit_profile.max_rss_bytes(), max_rss_bytes);

        let Some(expected_wall_time) = wall_time else { assert_eq!(unit_profile.wall_time(), None); return; };
        let actual_wall_time = unit_profile
            .wall_time()
            .expect("A wall_time should be present if provided to the constructor")
            .expect("No invalid f32 time states expected when constructing from Duration");
        assert_eq!(
            expected_wall_time.as_secs_f32(),
            actual_wall_time.as_secs_f32()
        );
    }

    // Build profile decoding from CSV
    #[test]
    fn build_profile_from_csv() {
        let csv_wo_time = &b"\
            file,max_rss\n\
            /path/to/lol.cpp,1234\n\
            /stuff.cpp,567"[..];
        let profile = super::from_csv_reader(csv::Reader::from_reader(csv_wo_time)).unwrap();
        assert_eq!(profile.len(), 2);
        assert_eq!(profile[0].rel_path(), Path::new("/path/to/lol.cpp"));
        assert_eq!(profile[0].max_rss_bytes(), 1234);
        assert_eq!(profile[0].wall_time(), None);
        assert_eq!(profile[1].rel_path(), Path::new("/stuff.cpp"));
        assert_eq!(profile[1].max_rss_bytes(), 567);
        assert_eq!(profile[1].wall_time(), None);

        let csv_with_time = &b"\
            file,max_rss,time\n\
            /a.cpp,0,0.0\n\
            /b.c++,567,NaN\n\
            /c.cxx,89000,+inf\n\
            /dd.cc,666666666,-0.1\n\
            /efg.c,888,18446800000000000000\n\
            /hhh.hpp,9000000001,18446700000000000000.9999999991\n"[..];
        let profile = super::from_csv_reader(csv::Reader::from_reader(csv_with_time)).unwrap();
        assert_eq!(profile.len(), 6);
        assert_eq!(profile[0].rel_path(), Path::new("/a.cpp"));
        assert_eq!(profile[0].max_rss_bytes(), 0);
        assert_eq!(profile[0].wall_time(), Some(Ok(Duration::default())));
        assert_eq!(profile[1].rel_path(), Path::new("/b.c++"));
        assert_eq!(profile[1].max_rss_bytes(), 567);
        assert!(profile[1].wall_time().unwrap().unwrap_err().is_nan());
        assert_eq!(profile[2].rel_path(), Path::new("/c.cxx"));
        assert_eq!(profile[2].max_rss_bytes(), 89000);
        assert_eq!(profile[2].wall_time(), Some(Err(f32::INFINITY)));
        assert_eq!(profile[3].rel_path(), Path::new("/dd.cc"));
        assert_eq!(profile[3].max_rss_bytes(), 666666666);
        assert_eq!(profile[3].wall_time(), Some(Err(-0.1)));
        assert_eq!(profile[4].rel_path(), Path::new("/efg.c"));
        assert_eq!(profile[4].max_rss_bytes(), 888);
        assert_eq!(profile[4].wall_time(), Some(Err(18446800000000000000.0)));
        assert_eq!(profile[5].rel_path(), Path::new("/hhh.hpp"));
        assert_eq!(profile[5].max_rss_bytes(), 9000000001);
        assert_eq!(
            profile[5].wall_time(),
            Some(Ok(Duration::from_secs_f32(18446700000000000000.9999999991)))
        );
    }
}
