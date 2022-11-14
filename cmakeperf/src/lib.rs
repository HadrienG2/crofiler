//! Full-build profiling based on compilation databases
//!
//! This crate can measure and process full-build performance profiles reporting
//! memory consumption and optionally compilation time with compilation unit
//! granularity.

pub mod commands;
pub mod measure;
pub mod output;
