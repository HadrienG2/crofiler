//! Test executable that follows a predefined pattern of CPU and RAM consumption
//!
//! Receives as input a number of <time>s:<mem>M pairs, such as 0.1s:50M. Each
//! pair indicates that the program should consume some amount of RAM for some
//! amount of time, in the above example 50 MB of RAM for 0.1 seconds.

use std::time::Duration;

const PAGE_SIZE: usize = 4096;

fn main() {
    for arg in std::env::args().skip(1) {
        // Decode how long we should hog memory
        let (time_secs, mem_and_suffix) = arg.split_once("s:").unwrap();
        let time = Duration::from_secs_f64(time_secs.parse().unwrap());

        // Decode how much memory we should hog
        let mem_mb = mem_and_suffix.strip_suffix('M').unwrap();
        let mem_mb: f64 = mem_mb.parse().unwrap();
        let mem_pages = (mem_mb * 1_000_000.0 / PAGE_SIZE as f64).ceil() as usize;

        // Create buffer and touch it to make sure Linux really allocates it
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
}
