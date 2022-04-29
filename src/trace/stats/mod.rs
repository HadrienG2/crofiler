//! Clang execution statistics that can be directly found in -ftime-trace data

pub mod activity;
pub mod global;

use serde_json as json;
use std::collections::HashMap;
use thiserror::Error;

/// What can go wrong while parsing TraceEvent arguments
#[derive(Error, Debug, PartialEq)]
pub enum ArgParseError {
    /// Encountered unexpected arguments
    #[error("got unexpected arguments {0:?}")]
    UnexpectedKeys(HashMap<Box<str>, json::Value>),

    /// Did not encounter expected argument
    #[error("did not get expected argument \"{0}\"")]
    MissingKey(&'static str),

    /// Got an unexpected value type for a certain argument
    #[error("got unexpected value for argument \"{0}\": {1:?}")]
    UnexpectedValue(&'static str, json::Value),
}
