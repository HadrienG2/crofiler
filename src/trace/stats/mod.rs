//! Clang execution statistics from -ftime-trace data

pub mod activity;
pub mod global;

use serde_json as json;
use std::collections::HashMap;
use thiserror::Error;

/// Error while parsing TraceEvent arguments
#[derive(Error, Debug, PartialEq)]
pub enum ArgParseError {
    #[error("got unexpected arguments {0:?}")]
    UnexpectedKeys(HashMap<String, json::Value>),

    #[error("expected argument \"{0}\" was not found")]
    MissingKey(&'static str),

    #[error("got unexpected value for argument \"{0}\": {1:?}")]
    UnexpectedValue(&'static str, json::Value),
}
