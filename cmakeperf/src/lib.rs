//! Full-build profiling based on compilation databases
//!
//! This crate can measure and process full-build performance profiles reporting
//! memory consumption and optionally compilation time with compilation unit
//! granularity.

pub mod commands;
pub mod measure;
pub mod output;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
