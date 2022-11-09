//! Parsing of metadata from clang's -ftime-trace output

use crate::ctf::events::metadata::{MetadataEvent, MetadataOptions, NameArgs};
use thiserror::Error;

/// Parse the clang process name
pub fn parse_process_name(m: MetadataEvent) -> Result<Box<str>, Box<NameParseError>> {
    match m {
        MetadataEvent::process_name {
            pid,
            args: NameArgs { name, extra },
            tid,
            options:
                MetadataOptions {
                    cat: Some(cat),
                    ts: Some(ts),
                    tts: None,
                },
        } if extra.is_empty()
            && cat.0.is_empty()
            && ts == 0.0
            && ((pid == 1 && tid == Some(0)) || tid == Some(pid)) =>
        {
            Ok(name)
        }
        _ => Err(Box::new(NameParseError::UnexpectedInput(m))),
    }
}

/// Parse the clang thread name
pub fn parse_thread_name(m: MetadataEvent) -> Result<Box<str>, Box<NameParseError>> {
    match m {
        MetadataEvent::thread_name {
            tid,
            args: NameArgs { name, extra },
            pid,
            options:
                MetadataOptions {
                    cat: Some(cat),
                    ts: Some(ts),
                    tts: None,
                },
        } if extra.is_empty() && cat.0.is_empty() && ts == 0.0 && pid == Some(tid) => Ok(name),
        _ => Err(Box::new(NameParseError::UnexpectedInput(m))),
    }
}

/// What can go wrong while parsing a name
#[derive(Error, Debug, PartialEq)]
pub enum NameParseError {
    /// Encountered unexpected input while trying to parse a clang name
    #[error("attempted to parse clang name from unexpected {0:#?}")]
    UnexpectedInput(MetadataEvent),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ctf::EventCategories, Pid};
    use serde_json as json;
    use std::collections::HashMap;

    fn test_name_parser(mock_name: &str, test_process_name: bool) {
        // Have a way to generate good and bad test inputs
        let name = Box::<str>::from(mock_name);
        let make_event = |good_type, pid: Option<Pid>, extra, tid, cat, ts, tts| {
            let args = NameArgs {
                name: name.clone(),
                extra,
            };
            let options = MetadataOptions { cat, ts, tts };
            if good_type ^ (!test_process_name) {
                MetadataEvent::process_name {
                    pid: pid.unwrap(),
                    args,
                    tid,
                    options,
                }
            } else {
                MetadataEvent::thread_name {
                    tid: tid.unwrap(),
                    args,
                    pid,
                    options,
                }
            }
        };

        // Valid parse_process_name input
        let tested_parser = if test_process_name {
            super::parse_process_name
        } else {
            super::parse_thread_name
        };
        if test_process_name {
            // Only the process_name parser accepts legacy clang 10 PID/TID duos
            assert_eq!(
                tested_parser(make_event(
                    true,
                    Some(1),
                    HashMap::new(),
                    Some(0),
                    Some(EventCategories::default()),
                    Some(0.0),
                    None
                )),
                Ok(name.clone())
            );
        }
        assert_eq!(
            tested_parser(make_event(
                true,
                Some(42),
                HashMap::new(),
                Some(42),
                Some(EventCategories::default()),
                Some(0.0),
                None
            )),
            Ok(name.clone())
        );

        // Various flavors of unexpected input
        let test_unexpected_input = |input: MetadataEvent| {
            assert_eq!(
                tested_parser(input.clone()),
                Err(Box::new(NameParseError::UnexpectedInput(input)))
            )
        };

        // Bad input type
        test_unexpected_input(make_event(
            false,
            Some(42),
            HashMap::new(),
            Some(42),
            Some(EventCategories::default()),
            Some(0.0),
            None,
        ));

        // Unexpected PID absence (thread_name specific)
        if !test_process_name {
            test_unexpected_input(make_event(
                true,
                None,
                HashMap::new(),
                Some(42),
                Some(EventCategories::default()),
                Some(0.0),
                None,
            ));
        }

        // Unexpected (PID, TID) pair
        test_unexpected_input(make_event(
            true,
            Some(42),
            HashMap::new(),
            Some(0),
            Some(EventCategories::default()),
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            Some(42),
            HashMap::new(),
            Some(43),
            Some(EventCategories::default()),
            Some(0.0),
            None,
        ));

        // Unexpected extra metadata
        test_unexpected_input(make_event(
            true,
            Some(42),
            maplit::hashmap! { "wtf".into() => json::json!("") },
            Some(42),
            Some(EventCategories::default()),
            Some(0.0),
            None,
        ));

        // Unexpected TID absence (process_name specific)
        if test_process_name {
            test_unexpected_input(make_event(
                true,
                Some(42),
                HashMap::new(),
                None,
                Some(EventCategories::default()),
                Some(0.0),
                None,
            ));
        }

        // Unexpected event categories metadata
        test_unexpected_input(make_event(
            true,
            Some(42),
            HashMap::new(),
            Some(42),
            Some(EventCategories(vec!["lol".into()].into_boxed_slice())),
            Some(0.0),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            Some(42),
            HashMap::new(),
            Some(42),
            None,
            Some(0.0),
            None,
        ));

        // Unexpected timestamp metadata
        test_unexpected_input(make_event(
            true,
            Some(42),
            HashMap::new(),
            Some(42),
            Some(EventCategories::default()),
            Some(4.2),
            None,
        ));
        test_unexpected_input(make_event(
            true,
            Some(42),
            HashMap::new(),
            Some(42),
            Some(EventCategories::default()),
            None,
            None,
        ));

        // Unexpected thread timestamp metadata
        test_unexpected_input(make_event(
            true,
            Some(42),
            HashMap::new(),
            Some(42),
            Some(EventCategories::default()),
            Some(0.0),
            Some(0.0),
        ));
    }

    #[test]
    fn parse_process_name() {
        test_name_parser("clang-12.0.1", true);
    }

    #[test]
    fn parse_thread_name() {
        test_name_parser("clang", false);
    }
}
