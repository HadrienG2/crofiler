//! CMake compilation database handling

use serde::Deserialize;
use serde_json as json;
use shlex::Shlex;
use std::{
    io,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// Load the compilation database
pub fn load() -> Result<Database, DatabaseLoadError> {
    let data = match std::fs::read_to_string(LOCATION) {
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            return Err(DatabaseLoadError::FileNotFound)
        }
        other => other?,
    };
    Ok(json::from_str::<Database>(&data)?)
}

/// Error that is emitted when an activity id cannot be displayed
#[derive(Debug, Error)]
pub enum DatabaseLoadError {
    /// Compilation database not found
    #[error("no compilation database found")]
    FileNotFound,

    /// Other I/O error
    #[error("failed to load compilation database ({0})")]
    IoError(#[from] io::Error),

    /// Failed to parse the compilation database
    #[error("failed to parse compilation database ({0})")]
    ParseError(#[from] json::Error),
}

/// One entry from the compilation database
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct Entry {
    directory: Box<Path>,
    command: String,
    file: Box<Path>,
}
//
impl Entry {
    /// Working directory
    pub fn current_dir(&self) -> &Path {
        &self.directory
    }

    /// Executable
    pub fn program(&self) -> impl AsRef<str> {
        self.full_args()
            .next()
            .expect("Compilation command contains no program")
    }

    /// Arguments to the executable
    pub fn args(&self) -> impl Iterator<Item = impl AsRef<str>> + '_ {
        self.full_args().skip(1)
    }

    /// Input file path
    pub fn input(&self) -> &Path {
        &self.file
    }

    /// Output file path
    ///
    /// This parses the arguments assuming a GCC-like `-o <output>` syntax.
    /// Will return None if basic syntax assumptions do not look fullfilled.
    ///
    pub fn output(&self) -> Option<PathBuf> {
        // Start from working directory provided by cmake
        let mut result = PathBuf::from(&*self.directory);

        // Parse arguments, extract file name and (assumed relative) path
        let rel_output = self
            .args()
            .skip_while(|arg| arg.as_ref() != "-o")
            .skip(1)
            .next()?;
        let rel_output = Path::new(rel_output.as_ref());
        let file_name = rel_output.file_name()?;

        // Add output path to working directory, try to canonicalize
        // (ignore failures to do so, that's not critical), add file name
        if let Some(rel_path) = rel_output.parent() {
            result.push(rel_path);
        }
        let res = result.canonicalize();
        std::mem::drop(res);
        result.push(file_name);

        // Emit result
        Some(result)
    }

    /// Command components
    fn full_args(&self) -> impl Iterator<Item = impl AsRef<str>> + '_ {
        Shlex::new(&self.command)
    }
}

/// Full compilation database
pub type Database = Vec<Entry>;

/// Location of the compilation database relative to the build directory
pub const LOCATION: &str = "compile_commands.json";
