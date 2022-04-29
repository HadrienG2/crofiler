//! Parsing of metadata from clang's -ftime-trace output

use crate::trace::ctf::events::metadata::{MetadataEvent, MetadataOptions, NameArgs};
use thiserror::Error;

/// Parse the clang process name
pub fn parse_process_name(m: MetadataEvent) -> Result<Box<str>, ProcessNameParseError> {
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
        } if extra.is_empty() && cat.0.is_empty() && ts == 0.0 => Ok(name),
        _ => Err(ProcessNameParseError::UnexpectedInput(m)),
    }
}

/// What can go wrong while parsing a process name
#[derive(Error, Debug, PartialEq)]
pub enum ProcessNameParseError {
    /// Encountered unexpected input while trying to parse the process name
    #[error("attempted to parse clang process name from unexpected {0:#?}")]
    UnexpectedInput(MetadataEvent),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::ctf::EventCategories;
    use serde_json as json;
    use std::collections::HashMap;

    #[test]
    fn parse_process_name() {
        // Have a way to generate good and bad test inputs
        let process_name = Box::<str>::from("clang");
        let make_event = |good_type, pid, extra, tid, cat, ts, tts| {
            let args = NameArgs {
                name: process_name.clone(),
                extra,
            };
            let options = MetadataOptions { cat, ts, tts };
            if good_type {
                MetadataEvent::process_name {
                    pid,
                    args,
                    tid,
                    options,
                }
            } else {
                MetadataEvent::thread_name {
                    tid: pid,
                    args,
                    pid: tid,
                    options,
                }
            }
        };

        // Valid parse_process_name input
        assert_eq!(
            super::parse_process_name(make_event(
                true,
                1,
                HashMap::new(),
                Some(0),
                Some(EventCategories::default()),
                Some(0.0),
                None
            )),
            Ok(process_name.clone())
        );

        // Various flavors of unexpected input
        let test_unexpected_input = |input: MetadataEvent| {
            assert_eq!(
                super::parse_process_name(input.clone()),
                Err(ProcessNameParseError::UnexpectedInput(input))
            )
        };
        test_unexpected_input(make_event(
            false,
            1,
            HashMap::new(),
            Some(0),
            Some(EventCategories::default()),
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            0,
            HashMap::new(),
            Some(0),
            Some(EventCategories::default()),
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            maplit::hashmap! { "wtf".into() => json::json!("") },
            Some(0),
            Some(EventCategories::default()),
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            HashMap::new(),
            Some(1),
            Some(EventCategories::default()),
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            HashMap::new(),
            Some(0),
            Some(EventCategories(vec!["lol".into()].into_boxed_slice())),
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            HashMap::new(),
            Some(0),
            None,
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            HashMap::new(),
            Some(0),
            Some(EventCategories::default()),
            Some(4.2),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            HashMap::new(),
            Some(0),
            Some(EventCategories::default()),
            None,
            None,
        ));
        test_unexpected_input(make_event(
            true,
            1,
            HashMap::new(),
            Some(0),
            Some(EventCategories::default()),
            Some(0.0),
            Some(0.0),
        ));
    }
}
