//! Test executable that follows a predefined pattern of CPU and RAM consumption
//!
//! Receives as input a number of <time>s:<mem>M pairs, such as 0.1s:50M. Each
//! pair indicates that the program should consume some amount of RAM for some
//! amount of time, in the above example 50 MB of RAM for 0.1 seconds.

use std::time::Duration;

fn main() {
    for arg in std::env::args().skip(1) {
        hog_cmd(&arg);
    }
}

/// Textual interface to `hog()`. Takes as input a `<time>s:<mem>M` pair such as
/// "0.1s:50M" and consumes the requested amount of amount of RAM for the
/// requested  amount of time, in the above example 50 MB of RAM for 0.1 seconds.
fn hog_cmd(cmd: &str) {
    // Decode how long we should hog memory
    let (time_secs, mem_and_suffix) = cmd.split_once("s:").unwrap();
    let time = Duration::from_secs_f64(time_secs.parse().unwrap());

    // Decode how much memory we should hog
    let mem_megs = mem_and_suffix.strip_suffix('M').unwrap();
    hog(time, mem_megs.parse().unwrap());
}

/// Hog onto a certain amount of memory for a certain amount of time
fn hog(time: Duration, ram_megs: usize) {
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
