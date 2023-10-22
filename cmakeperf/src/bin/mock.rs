//! Toy command processor that does just enough to test cmakeperf functionality
//!
//! Available commands are:
//!
//! - `hog [<mem>M:<time>s ...]` follows a programmed memory and time
//!   consumption pattern based on a set of key points. For example,
//!   `hog 50M:0.5s 0M:0.1s` will start by reaching a peak memory consumption
//!   of 50 MB over a period of 0.5s, then drop to 0 MB for 0.1s.
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
    time::{Duration, Instant},
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
            ("hog", cmd) => {
                hog_cmd(cmd);
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

/// Textual interface to `hog()`, see module-level docs
fn hog_cmd(cmd: &str) {
    let mut blocks = Vec::new();
    for keypoint in cmd.split(' ') {
        // Decode how much memory we should eventually hog
        let (mem_megs, duration_and_suffix) = keypoint
            .split_once("M:")
            .expect("Expected syntax <mem>M:<time>s");
        let mem_megs: usize = mem_megs
            .parse()
            .expect("Target memory amount should be an integer number of megabytes");
        let mem_blocks = (mem_megs as f64 * 1_000_000.0 / BLOCK_SIZE as f64).ceil() as usize;

        // Decode how long it should take us to reach that memory usage
        let duration_secs = duration_and_suffix
            .strip_suffix('s')
            .expect("Expected syntax <mem>M:<time>s");
        let duration = Duration::from_secs_f64(
            duration_secs
                .parse()
                .expect("Duration should be a floating-point second count"),
        );
        hog(&mut blocks, mem_blocks, duration);
    }
}

/// Reach a certain memory consumption over a certain amount of time
fn hog(blocks: &mut Vec<Block>, target_blocks: usize, duration: Duration) {
    // No need for ramp down memory usage when it decreases
    if target_blocks <= blocks.len() {
        blocks.truncate(target_blocks);
        std::hint::black_box(&mut *blocks);
        std::thread::sleep(duration);
        return;
    }

    // Prepare to increase memory usage
    blocks.reserve_exact(target_blocks);
    let initial_blocks = blocks.len();
    let mut grow = |target_blocks| {
        let new_blocks = target_blocks - blocks.len();
        blocks.extend(
            std::iter::repeat_with(|| {
                // Allocate a new memory block
                let mut block = vec![0u8; BLOCK_SIZE].into_boxed_slice();
                // Make sure each page of the block is actually allocated by the OS
                for page in 0..BLOCK_LEN {
                    // SAFETY: BLOCK_SIZE = BLOCK_LEN * PAGE_SIZE so all writes
                    //         are in bounds. This is only unsafe because
                    //         there's no safe way to volatile writes in std...
                    unsafe {
                        (&mut block[page * PAGE_SIZE] as *mut u8).write_volatile(1u8);
                    }
                }
                block
            })
            .take(new_blocks),
        );
        std::hint::black_box(&mut *blocks);
    };

    // Ramp up memory usage gradually
    let ramp_up_rate = (target_blocks - initial_blocks) as f64 / duration.as_secs_f64();
    let ramp_up_granularity = Duration::from_secs_f64(1.0 / ramp_up_rate);
    let start = Instant::now();
    loop {
        let elapsed = start.elapsed();
        if elapsed >= duration {
            break;
        }
        grow(initial_blocks + (elapsed.as_secs_f64() * ramp_up_rate).round() as usize);
        std::thread::sleep(ramp_up_granularity);
    }

    // Reach the memory usage setpoint
    grow(target_blocks);
}

/// Lower bound on the hardware paging side
///
/// This is the granularity of OS memory management tricks. One such trick, that
/// is a liability to us here, is that if an aligned contiguous block of memory
/// of size `PAGE_SIZE` is "allocated" (via e.g. malloc()) but not written to,
/// the OS may not truly allocate it because it's lazy.
///
const PAGE_SIZE: usize = 4096;

/// To reduce allocation overhead, on our side we manage memory with a coarser
/// granularity, a little less than a megabyte.
const BLOCK_LEN: usize = 1_000_000 / PAGE_SIZE;
const BLOCK_SIZE: usize = PAGE_SIZE * BLOCK_LEN;
type Block = Box<[u8]>;

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
    let mut file = NamedTempFile::new_in(workdir).expect("Failed to create child command file");
    write!(file, "{commands}").expect("Failed to write child command file");
    let path = file.into_temp_path();
    (
        Command::new(exe)
            .arg(&path)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("Failed to spawn child process"),
        path,
    )
}
