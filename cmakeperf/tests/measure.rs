//! Tests of the compilation profile measurement facility

use cmakeperf::commands::{CompilationDatabase, DatabaseEntry};
use std::{fs::File, io::Write, path::PathBuf, sync::Once};
use tempfile::{NamedTempFile, TempDir};

/// Test fixture for build profile measurement test
struct MeasurementTest {
    /// Temporary directory
    tmpdir: TempDir,

    /// List of compilation database entries
    entries: Vec<DatabaseEntry>,
}
//
impl MeasurementTest {
    /// Set up the compilation database builder and associated working directory
    ///
    /// A working directory must be imposed while running these tests because
    /// the measurement output uses relative file paths as job identifiers...
    ///
    pub fn new() -> Self {
        static SET_WORKDIR: Once = Once::new();
        SET_WORKDIR.call_once(|| {
            std::env::set_current_dir(Self::WORKDIR).expect("Failed to set working directory")
        });
        let tmpdir = TempDir::new_in(Self::WORKDIR).expect("Failed to create temporary directory");
        Self {
            tmpdir,
            entries: Vec::new(),
        }
    }

    /// Set up a mock compilation command performing certain actions
    ///
    /// Actions are described using the mini-language of the "mock" executable
    ///
    /// Returns the command's relative input file path, which will be used as an
    /// identifier in the measurement's output.
    ///
    pub fn add_job(&mut self, actions: &str) -> PathBuf {
        // Name of the mock executable
        const MOCK_EXE: &'static str = env!("CARGO_BIN_EXE_mock");

        // Set up a command file
        let (mut cmd_file, cmd_path) = self.make_tmpfile();
        writeln!(cmd_file, "{actions}").expect("Failed to write commands to file");
        let rel_cmd_path = pathdiff::diff_paths(&cmd_path, Self::WORKDIR)
            .expect("Failed to compute relative command file path");

        // Create a mock input file, check relative path to it
        let (_, input_path) = self.make_tmpfile();
        let rel_input_path = pathdiff::diff_paths(&input_path, Self::WORKDIR)
            .expect("Failed to compute relative input path");

        // Generate a compilation database entry running that command file
        let command = format!("{MOCK_EXE} {}", rel_cmd_path.display());
        self.entries
            .push(DatabaseEntry::new(self.tmpdir.path(), command, input_path));
        rel_input_path
    }

    /// Generate a compilation database with previously added commands
    ///
    /// You must keep the MeasurementTest struct alive as long as you want to use
    /// the output CompilationDatabase, else mock command files will be deleted.
    ///
    // TODO: Make private, provide a public measure() method instead
    pub fn make_db(&mut self) -> CompilationDatabase {
        CompilationDatabase::from_entries(self.entries.drain(..))
    }

    /// Create a named temporary file
    //
    // Since self.tmpdif is temporary, we could use a regular file in it. We're
    // just reusing the tempfile crate here for its random filename picking
    // logic. Hence the use of `keep()`, as we don't need another layer of
    // automatic deletion.
    //
    fn make_tmpfile(&self) -> (File, PathBuf) {
        let tmpfile = NamedTempFile::new_in(&self.tmpdir).expect("Failed to create temporary file");
        let (file, tmppath) = tmpfile.into_parts();
        (
            file,
            tmppath.keep().expect("Failed to persistify temporary file"),
        )
    }

    /// Imposed working directory when using `MeasurementTest`
    ///
    /// It is okay to impose a working directory in this multi-threaded test
    /// harness as long as all tests are actually agreeing on the same one.
    ///
    const WORKDIR: &'static str = env!("CARGO_TARGET_TMPDIR");
}

// TODO: Use simplelog to capture and check logs. Test with multi-layered
//       process trees, exit codes, various memory consumption patterns and
//       stdout/stderr output.

#[test]
fn basic_hog() {
    let mut test = MeasurementTest::new();
    let job = test.add_job("hog 50M:0.1s 150M:0.1s 10M:0.1s");
    let db = test.make_db();
    // TODO: Run measurement, check result
}
