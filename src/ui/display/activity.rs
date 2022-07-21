//! Utilities for displaying clang activities

use clang_time_trace::{ActivityArgument, ActivityId, ActivityTrace, CustomDisplay, MangledSymbol};
use std::io;
use thiserror::Error;
use unicode_width::UnicodeWidthStr;

/// Try to display an activity's name and argument in finite space
///
/// Returns Err(NotEnoughCols) if not even the activity name can fit in that
/// space. You may want to retry after eliminating other display elements if
/// they are deemed less important, or just display "…".
///
pub fn display_activity(
    mut output: impl io::Write,
    activity_id: ActivityId,
    activity_arg: &ActivityArgument,
    mut max_cols: u16,
) -> Result<(), ActivityIdError> {
    let activity_name: &str = activity_id.into();
    let has_argument = *activity_arg != ActivityArgument::Nothing;

    // Can we display at least ActivityName + (…) if there are parameters?
    if usize::from(max_cols) < activity_name.width() + 3 * (has_argument as usize) {
        // If not, error out
        return Err(ActivityIdError::NotEnoughCols(max_cols));
    } else {
        // If so, display the activity name...
        write!(output, "{activity_name}")?;
    }

    // If there are no parameters, stop here
    if !has_argument {
        return Ok(());
    }

    // Otheriwe, account for the reserved space and display the parameters
    max_cols -= activity_name.width() as u16 + 2;
    write!(output, "(")?;
    match activity_arg {
        ActivityArgument::UnnamedLoop => {
            super::display_string(&mut output, "<unnamed loop>", max_cols)?;
        }
        ActivityArgument::String(s)
        | ActivityArgument::MangledSymbol(MangledSymbol::Demangled(s))
        | ActivityArgument::MangledSymbol(MangledSymbol::Mangled(s)) => {
            super::display_string(&mut output, s, max_cols)?;
        }
        ActivityArgument::FilePath(p) => {
            write!(output, "{}", super::path::truncate_path(&p, max_cols))?;
        }
        ActivityArgument::CppEntity(e)
        | ActivityArgument::MangledSymbol(MangledSymbol::Parsed(e)) => {
            write!(output, "{}", e.bounded_display(max_cols))?;
        }
        ActivityArgument::Nothing => unreachable!(),
    }
    write!(output, ")")?;
    Ok(())
}
//
/// Error that is emitted when an activity id cannot be displayed
#[derive(Debug, Error)]
pub enum ActivityIdError {
    /// Not enough space to display activity name
    #[error("cannot display activity name in {0} terminal column(s)")]
    NotEnoughCols(u16),

    /// Output device errored out
    #[error("failed to write to output device ({0})")]
    IoError(#[from] io::Error),
}

#[cfg(test)]
mod tests {
    use crate::tests::TEST_TRACE;
    use assert_matches::assert_matches;

    use super::*;

    #[test]
    fn display_activity_id() {
        let mut display = Vec::new();
        let trace = TEST_TRACE.lock().unwrap();
        let mut check_display = |activity_trace: &ActivityTrace,
                                 max_cols,
                                 expected_display: &str| {
            display.clear();
            let result = super::display_activity(
                &mut display,
                activity_trace.activity().id(),
                &activity_trace.activity().argument(&trace),
                max_cols,
            );
            if expected_display.is_empty() {
                assert_eq!(
                    display,
                    b"",
                    "Expected empty/failing display, got {:?} (width = {:?})",
                    std::str::from_utf8(&display),
                    std::str::from_utf8(&display).map(|s| s.width())
                );
                assert_matches!(result, Err(ActivityIdError::NotEnoughCols(cols)) => assert_eq!(cols, max_cols));
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
                assert_matches!(result, Ok(()));
            }
        };

        {
            let execute_compiler = trace.root_activities().next().unwrap();
            check_display(&execute_compiler, 14, "");
            check_display(&execute_compiler, 15, "ExecuteCompiler");
        }

        // FIXME: Find a test dataset with an UnnamedLoop entry

        {
            let first_string_arg = trace
                .all_activities()
                .find(|activity| {
                    matches!(
                        activity.activity().argument(&trace),
                        ActivityArgument::String(_)
                    )
                })
                .unwrap();
            check_display(&first_string_arg, 9, "");
            check_display(&first_string_arg, 10, "RunPass(…)");
            check_display(&first_string_arg, 25, "RunPass(Loop Pa… Manager)");
            check_display(&first_string_arg, 26, "RunPass(Loop Pass Manager)");
        }

        {
            let first_demangled_arg = trace
                .all_activities()
                .find(|activity| {
                    matches!(
                        activity.activity().argument(&trace),
                        ActivityArgument::MangledSymbol(MangledSymbol::Demangled(_))
                    )
                })
                .unwrap();
            check_display(&first_demangled_arg, 29, "");
            check_display(&first_demangled_arg, 30, "PassManager<llvm::Function>(…)");
            check_display(&first_demangled_arg, 283, "PassManager<llvm::Function>(std::vector<Acts::detail_lt::IndexData, std::allocator<Acts::detail_lt::IndexData> >::_M_realloc_insert<>(__gnu_cxx::__normal_…erator<Acts::detail_lt::IndexData*, std::vector<Acts::detail_lt::IndexData, std::allocator<Acts::detail_lt::IndexData> > >, &&))");
            check_display(&first_demangled_arg, 284, "PassManager<llvm::Function>(std::vector<Acts::detail_lt::IndexData, std::allocator<Acts::detail_lt::IndexData> >::_M_realloc_insert<>(__gnu_cxx::__normal_iterator<Acts::detail_lt::IndexData*, std::vector<Acts::detail_lt::IndexData, std::allocator<Acts::detail_lt::IndexData> > >, &&))");
        }

        {
            let first_mangled_arg = trace
                .all_activities()
                .find(|activity| {
                    matches!(
                        activity.activity().argument(&trace),
                        ActivityArgument::MangledSymbol(MangledSymbol::Mangled(_))
                    )
                })
                .unwrap();
            check_display(&first_mangled_arg, 13, "");
            check_display(&first_mangled_arg, 14, "PromotePass(…)");
            check_display(&first_mangled_arg, 33, "PromotePass(__cxx_glo…l_var_init)");
            check_display(&first_mangled_arg, 34, "PromotePass(__cxx_global_var_init)");
        }

        {
            let first_path_arg = trace
                .all_activities()
                .find(|activity| {
                    matches!(
                        activity.activity().argument(&trace),
                        ActivityArgument::FilePath(_)
                    )
                })
                .unwrap();
            check_display(&first_path_arg, 8, "");
            check_display(&first_path_arg, 9, "Source(…)");
            check_display(&first_path_arg, 19, "Source(…)");
            check_display(&first_path_arg, 20, "Source(…/features.h)");
            check_display(&first_path_arg, 21, "Source(/…/features.h)");
            check_display(&first_path_arg, 24, "Source(/…/features.h)");
            check_display(&first_path_arg, 25, "Source(/usr/…/features.h)");
            check_display(&first_path_arg, 28, "Source(/usr/…/features.h)");
            check_display(&first_path_arg, 29, "Source(/…/include/features.h)");
            check_display(&first_path_arg, 30, "Source(/…/include/features.h)");
            check_display(&first_path_arg, 31, "Source(/usr/include/features.h)");
        }

        {
            let first_entity_arg = trace
                .all_activities()
                .find(|activity| {
                    matches!(
                        activity.activity().argument(&trace),
                        ActivityArgument::CppEntity(_)
                    )
                })
                .unwrap();
            check_display(&first_entity_arg, 12, "");
            check_display(&first_entity_arg, 13, "ParseClass(…)");
            check_display(&first_entity_arg, 25, "ParseClass(…)");
            check_display(&first_entity_arg, 26, "ParseClass(…::char_traits)");
            check_display(&first_entity_arg, 27, "ParseClass(…::char_traits)");
            check_display(&first_entity_arg, 28, "ParseClass(std::char_traits)");
        }

        {
            let first_parsed_mangled_arg = trace
                .all_activities()
                .find(|activity| {
                    matches!(
                        activity.activity().argument(&trace),
                        ActivityArgument::MangledSymbol(MangledSymbol::Parsed(_))
                    )
                })
                .unwrap();
            check_display(&first_parsed_mangled_arg, 10, "");
            check_display(&first_parsed_mangled_arg, 11, "SROAPass(…)");
            check_display(&first_parsed_mangled_arg, 24, "SROAPass(…)");
            check_display(&first_parsed_mangled_arg, 25, "SROAPass(…::test_method…)");
            check_display(&first_parsed_mangled_arg, 62, "SROAPass(…::test_method…)");
            check_display(
                &first_parsed_mangled_arg,
                63,
                "SROAPass(TrackFittingGainMatrixSmoother::Smooth::test_method())",
            );
        }
    }
}
