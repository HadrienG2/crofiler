//! Full-build profiling interface

use crate::{
    build::{
        clang,
        commands::{CompilationDatabase, DatabaseLoadError},
        profile::{self, cmakeperf, BuildProfile, ProfileLoadError},
    },
    ui::{
        display::path::truncate_path_iter,
        tui::{trace, with_state, CreatePrompt},
    },
    CliArgs,
};
use cursive::{
    traits::{Nameable, Resizable},
    views::{Dialog, LinearLayout, ProgressBar, TextView},
    Cursive, CursiveRunnable,
};
use cursive_table_view::{TableView, TableViewItem};
use std::{
    cmp::Ordering,
    io::{self, BufRead},
    path::Path,
    sync::{Arc, Mutex},
};

/// Perform full-build profiling, from which the user can go to trace profiling
pub fn profile(cursive: &mut CursiveRunnable, args: CliArgs) {
    // Load the compilation database
    let compilation_database = match CompilationDatabase::load() {
        Ok(database) => database,
        Err(e) => {
            let message = match e {
                DatabaseLoadError::FileNotFound => {
                    "No compilation database found. But it is needed for \
                    full-build profiling!\n\
                    Make sure you are in the build directory and/or re-run \
                    CMake with the -DCMAKE_EXPORT_COMPILE_COMMANDS=ON option."
                        .to_owned()
                }
                DatabaseLoadError::IoError(e) => {
                    format!("Failed to load compilation database: {e}")
                }
                DatabaseLoadError::ParseError(e) => {
                    format!("Failed to parse compilation database: {e}")
                }
            };
            error(cursive, message);
            return;
        }
    };

    // Check for availability of a suitable clang release
    let clangpp = match clang::find_clangpp() {
        Ok(program) => program,
        Err(e) => {
            error(
                cursive,
                format!("Did not find a clang version suitable for build profiling: {e}"),
            );
            return;
        }
    };

    // Determine the path to the build profile, (re)creating it if needed
    let profile_path =
        if let Some(profile_path) = update_profile(cursive, &args, &compilation_database) {
            profile_path
        } else {
            return;
        };

    // Load the full-build profile
    let profile = match profile::load(profile_path) {
        Ok(profile) => profile,
        Err(e) => {
            let message = match e {
                ProfileLoadError::FileNotFound => {
                    "Performance profile vanished before it could be loaded!".to_owned()
                }
                ProfileLoadError::ParseError(p) => {
                    format!("Failed to parse performance profile: {p}")
                }
            };
            error(cursive, message);
            return;
        }
    };

    // Display the build profile and let the user pick a trace to anaylze
    display_profile(cursive, args, compilation_database, clangpp, profile)
}

/// Update the build profile as needed as preparation for opening it
///
/// Returns the path to the build profile, which can be assumed to exist, or
/// None if we can't create a build profile and should stop here.
///
pub fn update_profile(
    cursive: &mut CursiveRunnable,
    args: &CliArgs,
    compilation_database: &CompilationDatabase,
) -> Option<Box<Path>> {
    // Determine full build profile path
    let default_profile_path = Path::new(profile::DEFAULT_LOCATION);
    let manually_specified = args.build_profile.is_some();
    let profile_path = args
        .build_profile
        .as_ref()
        .map(Path::new)
        .unwrap_or(default_profile_path);

    // Check if we should ask the user about full build profile (re)creation
    let freshness = match compilation_database.profile_freshness(profile_path) {
        Ok(freshness) => freshness,
        Err(e) => {
            error(
                cursive,
                format!("Failed to check build profile freshness: {e}"),
            );
            return None;
        }
    };
    let create_prompt = CreatePrompt::from_freshness(freshness, manually_specified, true);

    // Handle build profile creation requests
    if let Some(create_prompt) = create_prompt {
        if create_prompt.ask(cursive) {
            if !measure_profile(cursive, profile_path, compilation_database) {
                return None;
            }
        } else if !freshness.exists() {
            // If the user does not want to create a profile and none exists,
            // we have to stop there and terminate the program.
            return None;
        }
    }
    Some(profile_path.into())
}

/// Truth that a build profile is being displayed
pub fn is_profiling(cursive: &mut Cursive) -> bool {
    super::with_state(cursive, |state| state.showing_full_build)
}

/// Measure a build profile, storing the result in a specific location
///
/// Return true if everything worked out, false if that failed and the
/// application must terminate.
///
fn measure_profile(
    cursive: &mut CursiveRunnable,
    output_path: &Path,
    compilation_database: &CompilationDatabase,
) -> bool {
    // Check for cmakeperf availability
    let error_message = match cmakeperf::find() {
        Ok(status) if status.success() => None,
        Ok(bad_status) => Some(format!(
            "Found cmakeperf, but it exits with the error status {bad_status}.\n\
            Please repair your cmakeperf installation to enable build profiling."
        )),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Some(
            "cmakeperf was not found, please install it or bring it into PATH \
            to enable build profiling.\n\
            'pip3 install cmakeperf' should do this for you, but the use of a \
            virtual environment is recommended to avoid harmful interactions."
                .to_owned(),
        ),
        Err(e) => Some(format!(
            "Failed to run cmakeperf for build profiling due to an I/O error: {e}"
        )),
    };
    if let Some(message) = error_message {
        error(cursive, message);
        return false;
    }

    // If a cmakeperf output already exists, back it up
    let backup_path = output_path.with_extension("bak");
    match std::fs::rename(output_path, &backup_path) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::NotFound => {}
        Err(other) => {
            error(
                cursive,
                format!("Failed to back up previous build profile: {other}"),
            );
            return false;
        }
    }

    // If cmakeperf fails, we'll restore the backup if there is one or delete
    // the incomplete output file if there is no backup
    let restore_or_delete =
        |cursive: &mut CursiveRunnable| match std::fs::rename(backup_path, output_path)
            .or_else(|_| std::fs::remove_file(output_path))
        {
            Ok(()) => {}
            Err(e) if e.kind() == io::ErrorKind::NotFound => {}
            Err(other) => error(
                cursive,
                format!("Failed to clean up after cmakeperf: {other}"),
            ),
        };

    // Start cmakeperf
    let (mut collect, mut stdout) = match cmakeperf::Collect::start(output_path) {
        Ok(tuple) => tuple,
        Err(e) => {
            error(cursive, format!("Failed to start cmakeperf: {e}"));
            restore_or_delete(cursive);
            return false;
        }
    };

    // Set up the progress dialog
    with_state(cursive, |state| {
        state.no_escape = true;
    });
    let cb_sink = cursive.cb_sink().clone();
    let outcome = Arc::new(Mutex::new(None));
    let outcome2 = outcome.clone();
    cursive.add_layer(
        Dialog::around(
            LinearLayout::vertical()
                .child(TextView::new(
                    "Profiling the build, please minimize system activity...",
                ))
                .child(
                    ProgressBar::new()
                        // cmakeperf will print one line in the beginning and
                        // then one line per compilation database entry processed
                        .max(compilation_database.entries().count() + 1)
                        .with_task(move |counter| {
                            let mut line_buffer = String::new();
                            loop {
                                line_buffer.clear();
                                let outcome = match stdout.read_line(&mut line_buffer) {
                                    Ok(0) => Ok(()),
                                    Err(e) if e.kind() == io::ErrorKind::BrokenPipe => Ok(()),
                                    Ok(_nonzero) => {
                                        counter.tick(1);
                                        log::info!("cmakeperf output: {}", line_buffer.trim());
                                        continue;
                                    }
                                    Err(other_error) => Err(other_error),
                                };
                                *outcome2.lock().expect("Main thread has panicked") = Some(outcome);
                                break;
                            }
                            cb_sink
                                .send(Box::new(Cursive::quit))
                                .expect("Failed to exit cursive");
                        }),
                ),
        )
        .button("Abort", Cursive::quit),
    );
    let old_fps = cursive.fps();
    cursive.set_autorefresh(true);
    cursive.run();
    cursive.set_fps(old_fps.map(|u| u32::from(u)).unwrap_or(0));
    with_state(cursive, |state| {
        state.no_escape = false;
    });

    // Once we reach this point, the data collection process may either be done,
    // have errored out, or have been aborted by the user. We now need to
    // discriminate these various scenarios and react accordingly.
    let error_message = match outcome.lock() {
        Ok(mut guard) => match guard.take() {
            // Monitoring thread read process stdout through to the end, at this
            // point joining the process should be a trivial formality.
            Some(Ok(())) => match collect.finish() {
                // Note that we keep the profile backup around in case the
                // measurement was successful. That makes sense because
                // full-build profiles are tiny on disk and extremely expensive
                // to measure, so being able to go back to the old profile in
                // case the user is unsatisfied with this one is a good idea.
                Ok(()) => return true,
                Err(e) => Some(format!("Failed to join cmakeperf process: {e}")),
            },

            // The monitoring thread errored out. I can't think of a sane
            // situation where this should happen, but let's handle it anyway...
            Some(Err(e)) => Some(format!("Failed to collect build profile: {e}")),

            // The user has requested that data collection be aborted
            None => None,
        },

        // The thread monitoring the data collection stdout has panicked
        Err(e) => Some(format!("Build profile monitoring thread panicked: {e}")),
    };
    if let Some(message) = error_message {
        error(cursive, message);
    }

    // Next, try to kill the data collection process. This may race with the
    // process completing normally in the background, which is fine, but other
    // errors are not fine.
    match collect.kill() {
        Ok(()) => {}
        Err(e)
            if e.kind() == io::ErrorKind::InvalidInput || e.kind() == io::ErrorKind::NotFound => {}
        Err(other) => error(cursive, format!("Failed to kill cmakeperf: {other}")),
    }

    // After that, try to restore our backup of the previous build profile, if
    // it (still) exists, or failing that to at least delete any incomplete
    // profile produced by cmakeperf so we don't read it next time.
    restore_or_delete(cursive);
    false
}

/// Display the full-build profile, let user explore it and analyze traces
fn display_profile(
    cursive: &mut CursiveRunnable,
    args: CliArgs,
    compilation_database: CompilationDatabase,
    clangpp: impl AsRef<str> + 'static,
    profile: BuildProfile,
) {
    // Determine if the profile contains wall-clock time information
    let has_walltime = profile
        .first()
        .map(|unit| unit.wall_time().is_some())
        .unwrap_or(false);

    // Check terminal dimensions
    let (terminal_width, terminal_height) =
        termion::terminal_size().expect("Could not read terminal configuration");

    // Compute table column widths
    const MAX_RSS_WIDTH: usize = 10;
    const WALL_TIME_WIDTH: usize = 8;
    let mut non_file_width = MAX_RSS_WIDTH + 5;
    if has_walltime {
        non_file_width += WALL_TIME_WIDTH + 3;
    }
    let file_width = terminal_width as usize - non_file_width;

    // Set up table display
    type FullBuildProfileView = TableView<profile::Unit, ProfileColumn>;
    let mut table = FullBuildProfileView::new()
        .items(profile)
        .column(ProfileColumn::MaxRSS, "Memory", |c| {
            c.width(MAX_RSS_WIDTH).ordering(Ordering::Greater)
        })
        .default_column(ProfileColumn::MaxRSS);
    if has_walltime {
        table.add_column(ProfileColumn::WallTime, "Time", |c| {
            c.width(WALL_TIME_WIDTH).ordering(Ordering::Greater)
        });
    }
    table.add_column(
        ProfileColumn::RelPath(file_width as u16),
        "Source file",
        |c| c.width(file_width),
    );
    table.sort();
    table.set_selected_row(0);

    // Set up table interaction
    const TABLE_NAME: &str = "<full-build profile>";
    let time_trace_granularity = args.time_trace_granularity;
    table.set_on_submit(move |cursive, _row, index| {
        let rel_path: Box<Path> = cursive
            .call_on_name(TABLE_NAME, |view: &mut FullBuildProfileView| {
                view.borrow_item(index)
                    .expect("Callback shouldn't be called with an invalid index")
                    .rel_path()
                    .into()
            })
            .expect("Failed to access full-build profile view");
        trace::measure::show_wizard(
            cursive,
            &compilation_database,
            rel_path,
            clangpp.as_ref(),
            time_trace_granularity,
        );
    });
    let table = table.with_name(TABLE_NAME);

    // Update UI state
    super::with_state(cursive, |state| {
        state.showing_full_build = true;
    });

    // Show the profile
    cursive.add_fullscreen_layer(
        LinearLayout::vertical()
            .child(table.min_size((terminal_width, terminal_height - 1)))
            .child(TextView::new("Press H for help.")),
    );
    cursive.run();
}

/// Column in the build profile
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum ProfileColumn {
    /// Maximal RSS memory consumption
    MaxRSS,

    /// Elapsed CPU time
    WallTime,

    /// Relative file path (to be shrunk to a specified amount of term columns)
    RelPath(u16),
}
//
impl TableViewItem<ProfileColumn> for profile::Unit {
    fn to_column(&self, column: ProfileColumn) -> String {
        match column {
            ProfileColumn::MaxRSS => format!("{:.2} GB", self.max_rss_bytes() as f32 / 1e9),
            ProfileColumn::WallTime => format!(
                "{:.1}s",
                self.wall_time()
                    .expect("Wall-time should be present if this is probed")
                    .map(|dur| dur.as_secs_f32())
                    .unwrap_or_else(|raw_secs| raw_secs)
            ),
            ProfileColumn::RelPath(cols) => truncate_path_iter(
                self.rel_path()
                    .components()
                    .map(|s| s.as_os_str().to_string_lossy()),
                cols,
            )
            .into(),
        }
    }

    fn cmp(&self, other: &Self, column: ProfileColumn) -> Ordering
    where
        Self: Sized,
    {
        match column {
            ProfileColumn::MaxRSS => self.max_rss_bytes().cmp(&other.max_rss_bytes()),
            ProfileColumn::WallTime => self
                .wall_time()
                .zip(other.wall_time())
                .map(|(x, y)| x.partial_cmp(&y).expect("Failed to compare wall-time"))
                .expect("Wall-time should be present if this is probed"),
            ProfileColumn::RelPath(_cols) => self.rel_path().cmp(&other.rel_path()),
        }
    }
}

/// Simple error dialog, to be followed by application exit
fn error(cursive: &mut CursiveRunnable, message: impl Into<String>) {
    cursive.add_layer(Dialog::text(message).button("Quit", Cursive::quit));
    cursive.run();
}
