//! Toy command processor that does just enough to test cmakeperf functionality
//!
//! Available commands are:
//!
//! - `hog [<mem>M:<time>s ...]` follows a programmed memory consumption,
//!   at each step of which it consumes <mem> MB of RAM for <time> seconds.
//! - `stdout <line>` and `stderr <line>` print a line of output on stdout and
//!   stderr respectively.
//! - `exit <code>` sets the process' eventual exit code. This can only be done
//!   once. If not specified, the exit code is 0.
//! - `recurse <command>` takes one of the above commands as input and spawns a
//!   child process that executes it.
//!
//! If you want to recursively execute multiple commands, you can use the
//! multiline `recurse +` command, with the following syntax:
//!
//!
//! ```
//! recurse +
//!   <recursive command 1>
//!   <recursive command 2>
//!   ...
//!   <last recursive command>
//! <non-recursive command>
//! ```
//!
//! The first line after `recurse +` should start with a certain sequence of
//! indentation whitespace, to be replicated exactly at the start of each
//! following line. Once a line that does not start with this sequence or EOF is
//! reached, it is assumed that the list of recursive commands is over and the
//! newly encountered line (if any) is a non-recursive command, so the command
//! list is written down, the child process is started, and tthe presumed
//! command is processed normally.
//!
//! If this syntax sounds terrible, it's because it is loosely inspired by
//! Python's block syntax. Don't worry though, this is just a toy parser serving
//! as a test mock, you won't use it enough to hate it as much as Python.

use std::{
    fs::File,
    io::{BufRead, BufReader, Write},
    path::Path,
    process::{Child, Command, Stdio},
    time::Duration,
};
use tempfile::{NamedTempFile, TempPath};

fn main() {
    // Process execution environment
    let mut args = std::env::args();
    let exe = args.next().expect("Expected executable name as argv[0]");
    let filename = args.next().expect("Expected a command file name");
    assert_eq!(
        args.next(),
        None,
        "Expected nothing but a command file name"
    );
    let workdir = std::env::current_dir().expect("Failed to check working directory");

    // Prepare to process command file
    let command_file = BufReader::new(File::open(filename).expect("Failed to open command file"));
    let mut command_lines = command_file
        .lines()
        .map(|line| line.expect("Command file should be made of text lines"))
        .peekable();

    // Process command file
    let mut exit_code = None;
    let mut children = Vec::new();
    while let Some(line) = command_lines.next() {
        match line
            .split_once(' ')
            .expect("Expected command file syntax <command> <args>...")
        {
            ("hog", remainder) => {
                for cmd in remainder.split(' ') {
                    hog_cmd(cmd);
                }
            }

            ("stdout", remainder) => {
                println!("{remainder}");
            }
            ("stderr", remainder) => {
                eprintln!("{remainder}");
            }

            ("recurse", "+") => {
                let first_line = command_lines
                    .next()
                    .expect("Expected at least one child command after \"recurse +\"");
                let indentation = indentation(&first_line);
                let mut commands = first_line[indentation.len()..].to_string();
                while let Some(line) = command_lines.next_if(|line| line.starts_with(&indentation))
                {
                    commands.push('\n');
                    commands.push_str(&line[indentation.len()..]);
                }
                children.push(spawn_recurse(&exe, &workdir, &commands));
            }
            ("recurse", command) => {
                children.push(spawn_recurse(&exe, &workdir, command));
            }

            ("exit", remainder) => {
                assert_eq!(exit_code, None, "Exit code may only be set once");
                exit_code = Some(remainder.parse().expect("Expected integer exit code"));
            }

            (unknown, remainder) => {
                unreachable!("Got unknown command '{unknown}' with arguments '{remainder}'");
            }
        }
    }

    // Wait for child processes to terminate
    for (mut child, _input) in children {
        child.wait().expect("Failed to await child process");
    }
    std::process::exit(exit_code.unwrap_or(0));
}

/// Textual interface to `hog()`. Takes as input a `<mem>M:<time>s` pair such as
/// "50M:0.1s" and consumes the requested amount of amount of RAM for the
/// requested amount of time, in the above example 50 MB of RAM for 0.1 seconds.
fn hog_cmd(cmd: &str) {
    // Decode how much memory we should hog
    let (mem_megs, time_and_suffix) = cmd
        .split_once("M:")
        .expect("Expected syntax <mem>M:<time>s");
    let mem_megs = mem_megs
        .parse()
        .expect("Memory amount should be an integer number of megabytes");

    // Decode how long we should hog that memory
    let time_secs = time_and_suffix
        .strip_suffix('s')
        .expect("Expected syntax <mem>M:<time>s");
    let time = Duration::from_secs_f64(
        time_secs
            .parse()
            .expect("Duration should be a floating-point second count"),
    );
    hog(mem_megs, time);
}

/// Hog onto a certain amount of memory for a certain amount of time
fn hog(ram_megs: usize, time: Duration) {
    // Express memory amount in pages, aka the granularity of OS fuckery
    const PAGE_SIZE: usize = 4096;
    let mem_pages = (ram_megs as f64 * 1_000_000.0 / PAGE_SIZE as f64).ceil() as usize;

    // Create buffer and touch it to make sure the OS really does allocate it
    let mut alloc = vec![[0u8; PAGE_SIZE]; mem_pages];
    for page in &mut alloc {
        let first_byte = page.as_mut_slice() as *mut [u8] as *mut u8;
        unsafe {
            first_byte.write_volatile(1);
        }
    }

    // Sleep for the requested duration
    std::thread::sleep(time);
}

/// Extract the indentation whitespace from a line of text
fn indentation(s: &str) -> String {
    let result: String = s.chars().take_while(|c| c.is_whitespace()).collect();
    assert!(
        !result.is_empty(),
        "Must indent first recursive command after \"recurse +\""
    );
    assert!(
        result.len() != s.len(),
        "Expected command after \"recurse +\", not just whitespace"
    );
    result
}

/// Spawn a child process that runs the specified command list
fn spawn_recurse(exe: &str, workdir: &Path, commands: &str) -> (Child, TempPath) {
    let mut file = NamedTempFile::new_in(&workdir).expect("Failed to create child command file");
    write!(file, "{commands}").expect("Failed to write child command file");
    let path = file.into_temp_path();
    (
        Command::new(&exe)
            .arg(&path)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("Failed to spawn child process"),
        path,
    )
}
