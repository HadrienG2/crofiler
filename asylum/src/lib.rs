//! Tools for interning more than just strings

#![deny(missing_docs)]

pub mod path;
pub mod sequence;

/// Re-export used crates to avoid duplicate dependencies
pub use lasso;
