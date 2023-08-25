//! Utilities for displaying clang activities

use super::DisplayConfig;
use clang_time_trace::{ActivityArgument, ActivityId, CustomDisplay, DisplayState, Symbol};
use std::io;
use thiserror::Error;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// Try to display an activity's name and argument in finite space
///
/// If successful, returns truth that horizontal text wrapping should be enabled
/// on the display if possible. This will be the case for multi-line displays
/// except when line breaking is taken care of on our side (in which case
/// scrolling is better than wrapping).
///
/// Returns Err(NotEnoughCols) if not even the activity name can fit in that
/// space. You may want to retry after eliminating other display elements if
/// they are deemed less important, or just display "…".
///
pub fn display_activity_desc(
    mut output: impl io::Write,
    activity_id: &ActivityId,
    activity_arg: &ActivityArgument,
    mut config: DisplayConfig,
) -> Result<bool, ActivityDescError> {
    let activity_name = activity_id.name();
    let has_argument = *activity_arg != ActivityArgument::Nothing;
    let paren_width = '('.width().expect("Not a control char");
    debug_assert_eq!(Some(paren_width), ')'.width());

    // Can we display at least ActivityName + (…) if there are parameters?
    // FIXME: Remove allow-directive once pretty printing is implemented
    #[allow(unused_mut)]
    let mut should_wrap = match &mut config {
        // In the single-line case, this is only true if there are enough
        // terminal columns available, otherwise we error out
        DisplayConfig::SingleLine { max_cols } => {
            if usize::from(*max_cols) < activity_name.width() + 3 * (has_argument as usize) {
                return Err(ActivityDescError::NotEnoughCols(*max_cols));
            } else {
                write!(output, "{activity_name}")?;
                *max_cols =
                    max_cols.saturating_sub((activity_name.width() + 2 * paren_width) as u16);
            }
            false
        }

        // In the multi-line case this is always true, we only need to update
        // the header/trailer col count for the argument display (if any)
        DisplayConfig::MultiLine {
            header_cols,
            trailer_cols,
            ..
        } => {
            write!(output, "{activity_name}")?;
            *header_cols += (activity_name.width() + paren_width) as u16;
            *trailer_cols += paren_width as u16;
            true
        }
    };

    // If there are no parameters, stop here
    if !has_argument {
        return Ok(should_wrap);
    }

    // Otheriwe, account for the reserved space and display the parameters
    write!(output, "(")?;
    match activity_arg {
        ActivityArgument::UnnamedLoop => {
            super::display_string(&mut output, "<unnamed loop>", config)
        }
        ActivityArgument::String(s)
        | ActivityArgument::Symbol(Symbol::Demangled(s))
        | ActivityArgument::Symbol(Symbol::MaybeMangled(s)) => {
            super::display_string(&mut output, s, config)
        }
        ActivityArgument::FilePath(p) => {
            write!(output, "{}", super::path::display_path(p, config))
        }
        ActivityArgument::CppEntity(e) | ActivityArgument::Symbol(Symbol::Parsed(e)) => {
            match config {
                DisplayConfig::SingleLine { max_cols } => {
                    write!(output, "{}", e.bounded_display(max_cols))
                }
                DisplayConfig::MultiLine { .. } => {
                    // FIXME: Restore this once pretty-printing is implemented
                    /* should_wrap = false; */
                    write!(output, "{}", e.display(&DisplayState::default()))
                }
            }
        }
        ActivityArgument::Nothing => unreachable!("Handled above"),
    }?;
    write!(output, ")")?;
    Ok(should_wrap)
}

/// Error that is emitted when an activity id cannot be displayed
#[derive(Debug, Error)]
pub enum ActivityDescError {
    /// Not enough space to display activity name
    #[error("cannot display activity name in {0} terminal column(s)")]
    NotEnoughCols(u16),

    /// Output device errored out
    #[error("failed to write to output device ({0})")]
    IoError(#[from] io::Error),
}

#[cfg(test)]
mod tests {
    use crate::tests::with_test_trace;
    use assert_matches::assert_matches;
    use clang_time_trace::{
        ActivityArgumentType, ActivityTraceId, ClangTrace, ParsedActivityArgument, ParsedSymbol,
    };

    use super::*;

    #[test]
    fn display_activity_id() {
        let mut display = Vec::new();
        with_test_trace(|trace| {
            let mut check_display = |trace: &ClangTrace,
                                     (id, parsed_arg): &(
                ActivityTraceId,
                ParsedActivityArgument,
            ),
                                     max_cols,
                                     expected_display: &str| {
                display.clear();
                let result = super::display_activity_desc(
                    &mut display,
                    trace.activity_trace(*id).activity().id(),
                    &parsed_arg.resolve(trace),
                    // FIXME: Also test MultiLine once pretty printing is ironed out
                    DisplayConfig::SingleLine { max_cols },
                );
                if expected_display.is_empty() {
                    assert_eq!(
                        display,
                        b"",
                        "Expected empty/failing display, got {:?} (width = {:?})",
                        std::str::from_utf8(&display),
                        std::str::from_utf8(&display).map(|s| s.width())
                    );
                    assert_matches!(result, Err(ActivityDescError::NotEnoughCols(cols)) => assert_eq!(cols, max_cols));
                } else {
                    assert_eq!(
                        display,
                        expected_display.as_bytes(),
                        "Expected display {:?} (width = {}), got display {:?} (width = {:?})",
                        expected_display,
                        expected_display.width(),
                        std::str::from_utf8(&display),
                        std::str::from_utf8(&display).map(|s| s.width())
                    );
                    assert_matches!(result, Ok(false));
                }
            };

            {
                let id_and_parsed_arg;
                {
                    let execute_compiler = trace.root_activities().next().unwrap();
                    let id = execute_compiler.id();
                    let raw_arg = execute_compiler.activity().raw_argument().clone();
                    let parsed_arg = raw_arg.parse(trace).unwrap();
                    id_and_parsed_arg = (id, parsed_arg);
                }
                check_display(trace, &id_and_parsed_arg, 14, "");
                check_display(trace, &id_and_parsed_arg, 15, "ExecuteCompiler");
            }

            // Find a representative of every activity type in the test dataset
            // FIXME: Find a test dataset with an UnnamedLoop entry
            let mut first_string_arg = None;
            let mut first_path_arg = None;
            let mut first_entity_arg = None;
            let mut first_demangled_arg = None;
            let mut first_mangled_arg = None;
            let mut first_parsed_mangled_arg = None;
            {
                let mut remaining_args = 6;
                let activity_trace_ids = trace
                    .all_activities()
                    .map(|activity_trace| activity_trace.id())
                    .collect::<Vec<_>>();
                for id in activity_trace_ids {
                    // Exit once all desired activity types have been seen
                    if remaining_args == 0 {
                        break;
                    }

                    // Otherwise, check out what we have here
                    match trace
                        .activity_trace(id)
                        .activity()
                        .raw_argument()
                        .arg_type()
                    {
                        ActivityArgumentType::String => {
                            if !first_string_arg.is_some() {
                                let raw_arg =
                                    trace.activity_trace(id).activity().raw_argument().clone();
                                let parsed_arg = raw_arg.parse(trace).unwrap();
                                first_string_arg = Some((id, parsed_arg));
                                remaining_args -= 1;
                            }
                        }

                        ActivityArgumentType::FilePath => {
                            if !first_path_arg.is_some() {
                                let raw_arg =
                                    trace.activity_trace(id).activity().raw_argument().clone();
                                let parsed_arg = raw_arg.parse(trace).unwrap();
                                first_path_arg = Some((id, parsed_arg));
                                remaining_args -= 1;
                            }
                        }

                        ActivityArgumentType::CppEntity => {
                            if !first_entity_arg.is_some() {
                                let raw_arg =
                                    trace.activity_trace(id).activity().raw_argument().clone();
                                let parsed_arg = raw_arg.parse(trace).unwrap();
                                first_entity_arg = Some((id, parsed_arg));
                                remaining_args -= 1;
                            }
                        }

                        ActivityArgumentType::Symbol => {
                            if !first_mangled_arg.is_some()
                                || !first_demangled_arg.is_some()
                                || !first_parsed_mangled_arg.is_some()
                            {
                                let raw_arg =
                                    trace.activity_trace(id).activity().raw_argument().clone();
                                let parsed_arg = raw_arg.parse(trace).unwrap();
                                if let ParsedActivityArgument::Symbol(m) = &parsed_arg {
                                    match m {
                                        ParsedSymbol::Parsed(_)
                                            if first_parsed_mangled_arg.is_none() =>
                                        {
                                            first_parsed_mangled_arg = Some((id, parsed_arg));
                                            remaining_args -= 1;
                                        }
                                        ParsedSymbol::Demangled(_)
                                            if first_demangled_arg.is_none() =>
                                        {
                                            first_demangled_arg = Some((id, parsed_arg));
                                            remaining_args -= 1;
                                        }
                                        ParsedSymbol::MaybeMangled(_)
                                            if first_mangled_arg.is_none() =>
                                        {
                                            first_mangled_arg = Some((id, parsed_arg));
                                            remaining_args -= 1;
                                        }
                                        _ => {}
                                    }
                                } else {
                                    unreachable!()
                                }
                            }
                        }

                        _ => {}
                    }
                }
            }

            // Then test our findings

            {
                let first_string_arg = first_string_arg.unwrap();
                check_display(trace, &first_string_arg, 9, "");
                check_display(trace, &first_string_arg, 10, "RunPass(…)");
                check_display(trace, &first_string_arg, 25, "RunPass(Loop Pa… Manager)");
                check_display(trace, &first_string_arg, 26, "RunPass(Loop Pass Manager)");
            }

            {
                let first_path_arg = first_path_arg.unwrap();
                check_display(trace, &first_path_arg, 8, "");
                check_display(trace, &first_path_arg, 9, "Source(…)");
                check_display(trace, &first_path_arg, 19, "Source(…)");
                check_display(trace, &first_path_arg, 20, "Source(…/features.h)");
                check_display(trace, &first_path_arg, 21, "Source(/…/features.h)");
                check_display(trace, &first_path_arg, 24, "Source(/…/features.h)");
                check_display(trace, &first_path_arg, 25, "Source(/usr/…/features.h)");
                check_display(trace, &first_path_arg, 28, "Source(/usr/…/features.h)");
                check_display(trace, &first_path_arg, 29, "Source(/…/include/features.h)");
                check_display(trace, &first_path_arg, 30, "Source(/…/include/features.h)");
                check_display(
                    trace,
                    &first_path_arg,
                    31,
                    "Source(/usr/include/features.h)",
                );
            }

            {
                let first_entity_arg = first_entity_arg.unwrap();
                check_display(trace, &first_entity_arg, 12, "");
                check_display(trace, &first_entity_arg, 13, "ParseClass(…)");
                check_display(trace, &first_entity_arg, 25, "ParseClass(…)");
                check_display(trace, &first_entity_arg, 26, "ParseClass(…::char_traits)");
                check_display(trace, &first_entity_arg, 27, "ParseClass(…::char_traits)");
                check_display(trace, &first_entity_arg, 28, "ParseClass(std::char_traits)");
            }

            {
                let first_demangled_arg = first_demangled_arg.unwrap();
                check_display(trace, &first_demangled_arg, 16, "");
                check_display(trace, &first_demangled_arg, 17, "LoopUnrollPass(…)");
                check_display(trace, &first_demangled_arg, 217, "LoopUnrollPass(std::vector<std::bitset<36>, std::allocator<std::bitset<36> > >::_M_realloc_insert<>(__gnu_cxx::__no…al_iterator<std::bitset<36>*, std::vector<std::bitset<36>, std::allocator<std::bitset<36> > > >, &&))");
                check_display(trace, &first_demangled_arg, 218, "LoopUnrollPass(std::vector<std::bitset<36>, std::allocator<std::bitset<36> > >::_M_realloc_insert<>(__gnu_cxx::__normal_iterator<std::bitset<36>*, std::vector<std::bitset<36>, std::allocator<std::bitset<36> > > >, &&))");
            }

            {
                let first_mangled_arg = first_mangled_arg.unwrap();
                check_display(trace, &first_mangled_arg, 13, "");
                check_display(trace, &first_mangled_arg, 14, "PromotePass(…)");
                check_display(
                    trace,
                    &first_mangled_arg,
                    33,
                    "PromotePass(__cxx_glo…l_var_init)",
                );
                check_display(
                    trace,
                    &first_mangled_arg,
                    34,
                    "PromotePass(__cxx_global_var_init)",
                );
            }

            {
                let first_parsed_mangled_arg = first_parsed_mangled_arg.unwrap();
                check_display(trace, &first_parsed_mangled_arg, 10, "");
                check_display(trace, &first_parsed_mangled_arg, 11, "SROAPass(…)");
                check_display(trace, &first_parsed_mangled_arg, 24, "SROAPass(…)");
                check_display(
                    trace,
                    &first_parsed_mangled_arg,
                    25,
                    "SROAPass(…::test_method…)",
                );
                check_display(
                    trace,
                    &first_parsed_mangled_arg,
                    62,
                    "SROAPass(…::test_method…)",
                );
                check_display(
                    trace,
                    &first_parsed_mangled_arg,
                    63,
                    "SROAPass(TrackFittingGainMatrixSmoother::Smooth::test_method())",
                );
            }
        });
    }
}
