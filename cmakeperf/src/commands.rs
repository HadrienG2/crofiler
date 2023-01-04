//! CMake compilation database handling

use serde::Deserialize;
use serde_json as json;
use shlex::Shlex;
use std::{
    collections::HashMap,
    io,
    path::{Path, PathBuf},
    str::FromStr,
    time::Duration,
};
use thiserror::Error;

/// Full compilation database
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompilationDatabase(HashMap<Box<Path>, DatabaseEntry>);
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
        Ok(Self::from_str(&data)?)
    }

    /// List the database entries in arbitrary order
    pub fn entries(&self) -> impl Iterator<Item = &DatabaseEntry> {
        self.0.values()
    }

    /// Query a database entry by input file path
    ///
    /// Will return None if no file with that name exists in the database.
    ///
    pub fn entry(&self, input_path: &Path) -> Option<&DatabaseEntry> {
        self.0.get(input_path)
    }

    /// Check if a full-build profile seems up to date
    pub fn profile_freshness(&self, path: &Path) -> io::Result<ProductFreshness> {
        Self::product_freshness(self.entries().map(DatabaseEntry::input), path)
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
//
impl FromStr for CompilationDatabase {
    type Err = json::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let entries = json::from_str::<Vec<DatabaseEntry>>(s)?;
        Ok(Self(
            entries
                .into_iter()
                .map(|entry| (Box::from(entry.input()), entry))
                .collect(),
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

/// One entry from the compilation database
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct DatabaseEntry {
    /// Working directory for the build command
    directory: Box<Path>,

    /// Build command
    command: Box<str>,

    /// Input file
    file: Box<Path>,
}
//
impl DatabaseEntry {
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
        if let Ok(canonicalized) = result.canonicalize() {
            result = canonicalized;
        }
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
//
impl FromStr for DatabaseEntry {
    type Err = json::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        json::from_str(s)
    }
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

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use assert_matches::assert_matches;
    use fs_set_times::SystemTimeSpec;
    use std::{io, sync::Mutex};

    /// Working directory lock
    ///
    /// Tests that rely on a certain working directory configuration cannot run
    /// in parallel because the working directory is a process-wide
    /// configuration, not a thread-local variable. Use this to synchronize.
    ///
    pub struct WorkingDirectory;
    //
    impl WorkingDirectory {
        /// Set the current working directory
        pub fn set(&mut self, path: impl AsRef<Path>) {
            std::env::set_current_dir(path).unwrap()
        }
    }
    //
    pub static WORKING_DIRECTORY: Mutex<WorkingDirectory> = Mutex::new(WorkingDirectory);

    /// Mark a file as modified
    pub fn touch(path: impl AsRef<Path>) -> io::Result<()> {
        fs_set_times::set_times(
            path,
            Some(SystemTimeSpec::SymbolicNow),
            Some(SystemTimeSpec::SymbolicNow),
        )
    }

    /// Time to wait between touch() operations so that the timestamps differ
    pub const FS_CLOCK_GRANULARITY: Duration = Duration::from_millis(10);

    #[test]
    fn product_freshness() {
        assert!(!ProductFreshness::Nonexistent.exists());
        assert!(ProductFreshness::Outdated.exists());
        assert!(ProductFreshness::MaybeOutdated(None).exists());
        assert!(ProductFreshness::MaybeOutdated(Some(Duration::new(0, 0))).exists());
        assert!(ProductFreshness::MaybeOutdated(Some(Duration::new(0, 1))).exists());
    }

    #[test]
    fn database_entry() {
        use std::fs::File;

        // Check that even bad database entries produce reasonable properties
        let empty = DatabaseEntry {
            directory: Path::new("").into(),
            command: "".into(),
            file: Path::new("").into(),
        };
        assert_eq!(empty.current_dir(), Path::new(""));
        assert!(empty.program().is_none());
        assert_eq!(empty.args().count(), 0);
        assert_eq!(empty.input(), Path::new(""));
        assert_eq!(empty.output(), None);
        assert!(empty.derived_freshness(Path::new("/")).is_err());

        // Now try it with a more reasonable entry
        let input_dir = tempfile::tempdir().unwrap();
        let input_path = input_dir.path().join("input.cpp");
        File::create(&input_path).unwrap();
        //
        let output_base = tempfile::tempdir().unwrap();
        let output_subdir = Path::new("really");
        let output_dir = output_base.path().join(output_subdir);
        std::fs::create_dir(&output_dir).unwrap();
        let rel_output_dir = Path::new("really/../really");
        //
        let output_file = Path::new("out.o");
        let rel_output_path = rel_output_dir.join(output_file);
        let rel_output_path_str = format!("{}", rel_output_path.display());
        let abs_output_path = output_dir.join(output_file);
        //
        let command = format!(
            "SuperGoodCompiler --useless pointless -options -o {rel_output_path_str} -more fluff"
        );
        //
        let entry = DatabaseEntry {
            directory: output_base.path().into(),
            command: command.into(),
            file: input_path.clone().into(),
        };
        assert_eq!(entry.current_dir(), output_base.path());
        assert_eq!(entry.program().unwrap().as_ref(), "SuperGoodCompiler");
        let expected_args = [
            "--useless",
            "pointless",
            "-options",
            "-o",
            &rel_output_path_str,
            "-more",
            "fluff",
        ];
        assert_eq!(entry.args().count(), expected_args.len());
        for (actual, expected) in entry.args().zip(expected_args.into_iter()) {
            assert_eq!(actual.as_ref(), expected);
        }
        assert_eq!(entry.input(), input_path);
        assert_eq!(entry.output(), Some(abs_output_path.clone()));
        //
        let tmp_workdir = tempfile::tempdir().unwrap();
        {
            let mut workdir = WORKING_DIRECTORY.lock().unwrap();
            workdir.set(&tmp_workdir);
            File::create(CompilationDatabase::location()).unwrap();

            // Products may not exist yet
            let derived_path = output_base.path().join("stuff.json");
            assert_eq!(
                entry.derived_freshness(&derived_path).unwrap(),
                ProductFreshness::Nonexistent
            );

            // Products may be apparently up to date
            File::create(&derived_path).unwrap();
            assert_matches!(
                entry.derived_freshness(&derived_path).unwrap(),
                ProductFreshness::MaybeOutdated(Some(_))
            );

            // Products may be outdated with respect to inputs...
            std::thread::sleep(FS_CLOCK_GRANULARITY);
            touch(&input_path).unwrap();
            assert_eq!(
                entry.derived_freshness(&derived_path).unwrap(),
                ProductFreshness::Outdated
            );

            // ...and can be updated back to MaybeOutdated state
            touch(&derived_path).unwrap();
            assert_matches!(
                entry.derived_freshness(&derived_path).unwrap(),
                ProductFreshness::MaybeOutdated(Some(_))
            );

            // Products may be outdated with respect to the compilation database
            std::thread::sleep(FS_CLOCK_GRANULARITY);
            touch(CompilationDatabase::location()).unwrap();
            assert_eq!(
                entry.derived_freshness(&derived_path).unwrap(),
                ProductFreshness::Outdated
            );
        }
    }

    // TODO: ...then test CompilationDatabase, reusing some of the above mock logic
}
