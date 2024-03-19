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
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CompilationDatabase(HashMap<Box<Path>, DatabaseEntry>);
//
impl CompilationDatabase {
    /// Location of the compilation database relative to the build directory
    pub fn location() -> &'static Path {
        Path::new("compile_commands.json")
    }

    /// Create a compilation database from a bunch of entries
    pub fn from_entries(entries: impl IntoIterator<Item = DatabaseEntry>) -> Self {
        Self(
            entries
                .into_iter()
                .map(|entry| (Box::from(entry.input()), entry))
                .collect(),
        )
    }

    /// Load the compilation database from the working directory
    pub fn load() -> Result<Self, DatabaseLoadError> {
        let data = match std::fs::read_to_string(Self::location()) {
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                return Err(DatabaseLoadError::FileNotFound)
            }
            other => other?,
        };
        Ok(Self::from_str(&data)?)
    }

    /// Create an empty compilation database
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Add an entry to an existing compilation database
    pub fn push(&mut self, entry: DatabaseEntry) {
        assert_eq!(
            self.0.insert(Box::from(entry.input()), entry),
            None,
            "Duplicate entry in compilation database"
        );
    }

    /// Remove all entries from this compilation database
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// List the database entries in arbitrary order
    pub fn entries(&self) -> impl Iterator<Item = &DatabaseEntry> {
        self.0.values()
    }

    /// Query a database entry by input file path
    ///
    /// Will return None if no file with that name exists in the database.
    ///
    /// Note that the filename matching is not smart and just looks for an exact
    /// string match against entry.path().
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
        Ok(Self::from_entries(entries))
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
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct DatabaseEntry {
    /// Working directory for the build command
    directory: Box<Path>,

    /// Build command
    command: Box<str>,

    /// Input file
    file: Box<Path>,

    /// Output file (newer CMake only)
    output: Option<Box<Path>>,
}
//
impl DatabaseEntry {
    /// Create a new database entry
    pub fn new(
        directory: impl Into<Box<Path>>,
        command: impl Into<Box<str>>,
        file: impl Into<Box<Path>>,
        output: Option<PathBuf>,
    ) -> Self {
        Self {
            directory: directory.into(),
            command: command.into(),
            file: file.into(),
            output: output.map(|out| out.into_boxed_path()),
        }
    }

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
        // If CMake gives us the output path, use it
        if let Some(out) = self.output.as_ref() {
            return Some(out.to_path_buf());
        }

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
    use std::{fs::File, io::Write, sync::Mutex};

    /// Working directory lock
    ///
    /// Tests that rely on a certain working directory configuration cannot run
    /// in parallel because the working directory is a process-wide
    /// configuration, not a thread-local variable. Use this to synchronize.
    ///
    pub struct WorkingDirectory;
    //
    impl WorkingDirectory {
        /// Run a serios of operations in a certain working directory
        pub fn with(&mut self, path: impl AsRef<Path>, operation: impl FnOnce()) {
            let old_workdir = std::env::current_dir().unwrap();
            std::env::set_current_dir(path).unwrap();
            operation();
            std::env::set_current_dir(old_workdir).unwrap();
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
        // Set up a basic compilation database mock
        let tmp_workdir = tempfile::tempdir().unwrap();
        File::create(tmp_workdir.path().join(CompilationDatabase::location())).unwrap();

        // Check that even crazy database entries produce reasonable properties
        let empty = DatabaseEntry {
            directory: Path::new("/").into(),
            command: "xxx".into(),
            file: Path::new("/etc/fstab").into(),
            output: None,
        };
        assert_eq!(empty.current_dir(), Path::new("/"));
        assert_eq!(empty.program().unwrap().as_ref(), "xxx");
        assert_eq!(empty.args().count(), 0);
        assert_eq!(empty.input(), Path::new("/etc/fstab"));
        assert_eq!(empty.output(), None);
        WORKING_DIRECTORY.lock().unwrap().with(&tmp_workdir, || {
            assert_eq!(
                empty.derived_freshness(Path::new("/")).unwrap(),
                ProductFreshness::Outdated
            );
        });

        // Now try it with a more reasonable entry
        let tmp_input_dir = tempfile::tempdir().unwrap();
        let input_path = tmp_input_dir.path().join("input.cpp");
        File::create(&input_path).unwrap();
        //
        let tmp_output_base = tempfile::tempdir().unwrap();
        let output_subdir = Path::new("really");
        let output_dir = tmp_output_base.path().join(output_subdir);
        std::fs::create_dir(&output_dir).unwrap();
        let rel_output_dir = output_dir.join(Path::new("..")).join(&output_dir);
        //
        let output_file = Path::new("out.o");
        let rel_output_path = rel_output_dir.join(output_file);
        let rel_output_path_str = format!("{}", rel_output_path.display());
        let abs_output_path = output_dir.canonicalize().unwrap().join(output_file);
        //
        let command = format!(
            "SuperGoodCompiler --useless pointless -options -o {rel_output_path_str} -more fluff"
        );
        //
        let entry = DatabaseEntry {
            directory: tmp_output_base.path().into(),
            command: command.into(),
            file: input_path.clone().into(),
            output: None,
        };
        assert_eq!(entry.current_dir(), tmp_output_base.path());
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
        WORKING_DIRECTORY.lock().unwrap().with(&tmp_workdir, || {
            // Products may not exist yet
            let derived_path = tmp_output_base.path().join("stuff.json");
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
        });
    }

    #[test]
    fn compilation_database() {
        let tmp_input_dir_1 = tempfile::tempdir().unwrap();
        let input_path_1 = tmp_input_dir_1.path().join("a.cpp");
        File::create(&input_path_1).unwrap();
        let tmp_output_dir_1 = tempfile::tempdir().unwrap();
        let cmd_1 = format!("cc -o a.o {}", input_path_1.display());

        let tmp_input_dir_2 = tempfile::tempdir().unwrap();
        let input_path_2 = tmp_input_dir_2.path().join("b.hxx");
        File::create(&input_path_2).unwrap();
        let tmp_output_dir_2 = tempfile::tempdir().unwrap();
        let cmd_2 = format!("not_a_compiler {}", input_path_2.display());

        let tmp_workdir = tempfile::tempdir().unwrap();
        {
            let mut database_json =
                File::create(tmp_workdir.path().join(CompilationDatabase::location())).unwrap();
            write!(
                database_json,
                "[\
                {{\"directory\":\"{}\",\"command\":\"{cmd_1}\",\"file\":\"{}\"}},\
                {{\"directory\":\"{}\",\"command\":\"{cmd_2}\",\"file\":\"{}\"}}\
                ]",
                tmp_output_dir_1.path().display(),
                input_path_1.display(),
                tmp_output_dir_2.path().display(),
                input_path_2.display()
            )
            .unwrap();
        }

        WORKING_DIRECTORY.lock().unwrap().with(&tmp_workdir, || {
            let db = CompilationDatabase::load().unwrap();

            assert_eq!(db.entries().count(), 2);
            let entry1 = db.entry(&input_path_1).unwrap();
            assert_eq!(entry1.current_dir(), tmp_output_dir_1.path());
            assert_eq!(entry1.raw_command(), cmd_1);
            assert_eq!(entry1.input(), &input_path_1);
            let entry2 = db.entry(&input_path_2).unwrap();
            assert_eq!(entry2.current_dir(), tmp_output_dir_2.path());
            assert_eq!(entry2.raw_command(), cmd_2);
            assert_eq!(entry2.input(), &input_path_2);

            let profile_path = Path::new("my_super_build_profile.csv");
            assert_eq!(
                db.profile_freshness(profile_path).unwrap(),
                ProductFreshness::Nonexistent
            );

            File::create(profile_path).unwrap();
            assert_matches!(
                db.profile_freshness(profile_path).unwrap(),
                ProductFreshness::MaybeOutdated(Some(_))
            );

            std::thread::sleep(FS_CLOCK_GRANULARITY);
            touch(CompilationDatabase::location()).unwrap();
            assert_eq!(
                db.profile_freshness(profile_path).unwrap(),
                ProductFreshness::Outdated
            );

            touch(profile_path).unwrap();
            assert_matches!(
                db.profile_freshness(profile_path).unwrap(),
                ProductFreshness::MaybeOutdated(Some(_))
            );

            std::thread::sleep(FS_CLOCK_GRANULARITY);
            touch(&input_path_1).unwrap();
            assert_eq!(
                db.profile_freshness(profile_path).unwrap(),
                ProductFreshness::Outdated
            );

            touch(profile_path).unwrap();
            assert_matches!(
                db.profile_freshness(profile_path).unwrap(),
                ProductFreshness::MaybeOutdated(Some(_))
            );

            std::thread::sleep(FS_CLOCK_GRANULARITY);
            touch(&input_path_2).unwrap();
            assert_eq!(
                db.profile_freshness(profile_path).unwrap(),
                ProductFreshness::Outdated
            );
        });
    }
}
