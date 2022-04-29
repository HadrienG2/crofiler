//! Parsing of metadata from clang's -ftime-trace output

use crate::trace::ctf::events::metadata::{MetadataEvent, MetadataOptions, NameArgs};
use thiserror::Error;

/// Parse the clang process name
pub fn parse_process_name(m: &MetadataEvent) -> Result<String, ProcessNameParseError> {
    match m {
        MetadataEvent::process_name {
            pid: 1,
            args: NameArgs { name, extra },
            tid: Some(0),
            options:
                MetadataOptions {
                    cat: Some(cat),
                    ts: Some(ts),
                    tts: None,
                },
        } if extra.is_empty() && cat.0.is_empty() && *ts == 0.0 => Ok(name.clone()),
        _ => Err(ProcessNameParseError::UnexpectedInput(m.clone())),
    }
}

/// What can go wrong while parsing a process name
#[derive(Error, Debug, PartialEq)]
pub enum ProcessNameParseError {
    #[error("attempted to parse clang process name from unexpected {0:#?}")]
    UnexpectedInput(MetadataEvent),
}

// FIXME: Add tests
