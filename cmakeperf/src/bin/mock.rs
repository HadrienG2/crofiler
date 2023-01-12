//! Toy command processor that does just enough to test cmakeperf functionality

use std::{
    fs::File,
    io::{BufRead, BufReader},
    process::{Command, Stdio},
    time::Duration,
};

fn main() {
    // Parse argument
    let mut args = std::env::args().skip(1);
    let filename = args.next().expect("Expected a command file name");
    assert_eq!(
        args.next(),
        None,
        "Expected nothing but a command file name"
    );

    // Process command file
    let command_file = BufReader::new(File::open(filename).expect("Failed to open command file"));
    let mut exit_code = None;
    let mut children = Vec::new();
    for line in command_file.lines() {
        match line
            .expect("Expected command file to be made of text lines")
            .split_once(' ')
            .expect("Expected command file syntax <command> <args>...")
        {
            // Use up memory and CPU time according to a predefined pattern
            ("hog", remainder) => {
                for cmd in remainder.split(' ') {
                    hog_cmd(cmd);
                }
            }

            // Print a line of output on stdout or stderr
            ("stdout", remainder) => {
                println!("{remainder}");
            }
            ("stderr", remainder) => {
                eprintln!("{remainder}");
            }

            // Spawn a child process
            ("spawn", remainder) => {
                let args = shlex::split(remainder).expect("Expected a shell command");
                children.push(
                    Command::new(&args[0])
                        .args(args.into_iter().skip(1))
                        .stdin(Stdio::null())
                        .stdout(Stdio::null())
                        .stderr(Stdio::null())
                        .spawn()
                        .expect("Failed to spawn child process"),
                );
            }

            // Set the exit code (0 by default)
            ("exit", remainder) => {
                assert_eq!(exit_code, None, "Exit code may only be set once");
                exit_code = Some(remainder.parse().expect("Expected integer exit code"));
            }

            // Should not happen
            (unknown, remainder) => {
                unreachable!("Got unknown command '{unknown}' with arguments '{remainder}'");
            }
        }
    }

    // Wait for child processes to terminate
    for mut child in children {
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
