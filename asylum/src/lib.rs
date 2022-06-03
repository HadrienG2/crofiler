//! Tools for interning more than just strings

#![deny(missing_docs)]

pub mod path;
pub mod sequence;

/// Re-export of the lasso version that we use to avoid duplication
pub use lasso;
