//! CMake compilation database handling

use serde::Deserialize;
use serde_json as json;
use shlex::Shlex;
use std::{
    collections::HashMap,
    io,
    path::{Path, PathBuf},
    time::Duration,
};
use thiserror::Error;

/// One entry from the compilation database
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct Entry {
    /// Working directory for the build command
    directory: Box<Path>,

    /// Build command
    command: String,

    /// Input
    file: Box<Path>,
}
//
impl Entry {
    /// Working directory
    pub fn current_dir(&self) -> &Path {
        &self.directory
    }

    /// Executable
    #[allow(unused)]
    pub fn program(&self) -> Option<impl AsRef<str>> {
        self.full_args().next()
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
        let rel_output = self.args().skip_while(|arg| arg.as_ref() != "-o").nth(1)?;
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

    /// Check if a file derived from this source file seems up to date
    pub fn derived_freshness(&self, output_path: &Path) -> io::Result<ProductFreshness> {
        CompilationDatabase::product_freshness(std::iter::once(self.input()), output_path)
    }

    /// Command components
    fn full_args(&self) -> impl Iterator<Item = impl AsRef<str>> + '_ {
        Shlex::new(self.raw_command())
    }

    /// Raw compilation command without further processing
    pub fn raw_command(&self) -> &str {
        &self.command
    }
}

/// Full compilation database
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompilationDatabase(HashMap<Box<Path>, Entry>);
//
impl CompilationDatabase {
    /// Location of the compilation database relative to the build directory
    pub fn location() -> &'static Path {
        Path::new("compile_commands.json")
    }

    /// Load from working directory
    pub fn load() -> Result<Self, DatabaseLoadError> {
        let data = match std::fs::read_to_string(Self::location()) {
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                return Err(DatabaseLoadError::FileNotFound)
            }
            other => other?,
        };
        let entries = json::from_str::<Vec<Entry>>(&data)?;
        Ok(Self(
            entries
                .into_iter()
                .map(|entry| (Box::from(entry.input()), entry))
                .collect(),
        ))
    }

    /// List the database entries in arbitrary order
    pub fn entries(&self) -> impl Iterator<Item = &Entry> {
        self.0.values()
    }

    /// Query a database entry by input file path
    ///
    /// Will return None if no file with that name exists in the database.
    ///
    pub fn entry(&self, input_path: &Path) -> Option<&Entry> {
        self.0.get(input_path)
    }

    /// Check if a full-build profile seems up to date
    pub fn profile_freshness(&self, path: &Path) -> io::Result<ProductFreshness> {
        Self::product_freshness(self.entries().map(Entry::input), path)
    }

    /// Check if some build derivative seems up to date
    fn product_freshness<'a>(
        inputs: impl Iterator<Item = &'a Path>,
        output: impl AsRef<Path>,
    ) -> io::Result<ProductFreshness> {
        // Check build product existence and mtime
        let output = output.as_ref();
        let product_mtime = match output.metadata().and_then(|m| m.modified()) {
            Ok(mtime) => mtime,
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                return Ok(ProductFreshness::Nonexistent)
            }
            Err(other) => return Err(other),
        };

        // Compare product mtime to compilation database mtime
        if product_mtime < Path::new(Self::location()).metadata()?.modified()? {
            return Ok(ProductFreshness::Outdated);
        }

        // Compare to mtime of every input file
        for input in inputs {
            if product_mtime < input.metadata()?.modified()? {
                return Ok(ProductFreshness::Outdated);
            }
        }

        // So far, so good, but we don't know about all build dependencies so
        // we should stay cautious in our conclusions.
        Ok(ProductFreshness::MaybeOutdated(
            product_mtime.elapsed().ok(),
        ))
    }
}

/// Failure to load the CompilationDatabase from disk
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

/// Result of a build profile/output freshness query
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProductFreshness {
    /// Build product has not been produced yet
    Nonexistent,

    /// Build product exists, but is provably outdated
    Outdated,

    /// Build profile has existed for a certain time and could be outdated
    ///
    /// None will be used to encode the case where the build product age is
    /// unknown, which can happen when the system time is inconsistent with
    /// filesystem timestamps and the build product seems to be from the future.
    ///
    MaybeOutdated(Option<Duration>),
}
//
impl ProductFreshness {
    /// Truth that a build product exists, however stale
    pub fn exists(&self) -> bool {
        match self {
            ProductFreshness::Nonexistent => false,
            ProductFreshness::Outdated => true,
            ProductFreshness::MaybeOutdated(_age) => true,
        }
    }
}
