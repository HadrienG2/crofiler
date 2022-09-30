//! Full-build profiling interface

use crate::{
    build::{
        clang,
        commands::{CompilationDatabase, DatabaseLoadError, ProductFreshness},
        profile::{
            self,
            cmakeperf::{self, CollectStep},
            BuildProfile, ProfileLoadError,
        },
    },
    ui::display::path::truncate_path_iter,
    CliArgs,
};
use cursive::{
    traits::Resizable,
    views::{Dialog, LinearLayout, ProgressBar, TextView},
    Cursive, CursiveRunnable,
};
use cursive_table_view::{TableView, TableViewItem};
use std::{
    cell::Cell,
    cmp::Ordering,
    fmt::Write,
    io,
    path::Path,
    rc::Rc,
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
    display_profile(cursive, compilation_database, clangpp.as_ref(), profile)
}

/// Update the build profile as needed as preparation for opening it
///
/// Returns the path to the build profile, which can be assumed to exist, or
/// None if we can't create a build profile and should stop here.
///
// TODO: Make some of this logic reusable for traces
pub fn update_profile(
    cursive: &mut CursiveRunnable,
    args: &CliArgs,
    compilation_database: &CompilationDatabase,
) -> Option<Box<Path>> {
    // Determine full build profile path
    let default_profile_path = Path::new(profile::DEFAULT_LOCATION);
    let is_default_path = args.build_profile.is_none();
    let profile_path = args
        .build_profile
        .as_ref()
        .map(Path::new)
        .unwrap_or(default_profile_path);

    // Check if we should ask the user about full build profile (re)creation
    let (profile_exists, create_prompt) = match compilation_database.profile_freshness(profile_path)
    {
        // No build profiled at the expected location
        Ok(ProductFreshness::Nonexistent) => (false, Some(CreatePrompt::new_build_profile())),

        // There is a build profile but it is obviously stale
        Ok(ProductFreshness::Outdated) => (true, Some(CreatePrompt::stale_build_profile())),

        // There is a build profile, and it does not look obviously stale, but
        // might still be because we are not omniscient. Prompt if it's older
        // than one minute and we're using the default path (not manual choice)
        Ok(ProductFreshness::MaybeOutdated(age)) => {
            let age_mins = age.map(|d| d.as_secs() / 60).unwrap_or(u64::MAX);
            let create_prompt = if age_mins > 0 && is_default_path {
                Some(CreatePrompt::maybe_stale_build_profile(Some(age_mins)))
            } else {
                None
            };
            (true, create_prompt)
        }

        // Something wrong happened while checking
        Err(e) => {
            error(
                cursive,
                format!("Failed to check build profile freshness: {e}"),
            );
            return None;
        }
    };

    // Handle build profile creation requests
    if let Some(create_prompt) = create_prompt {
        let should_create = create_prompt.ask(cursive);
        if should_create {
            if !measure_profile(cursive, profile_path, compilation_database) {
                return None;
            }
        } else if !profile_exists {
            // If the user does not want to create a profile and none exists,
            // we have to stop there and terminate the program.
            return None;
        }
    }
    Some(profile_path.into())
}

/// Prompt to be shown when a new profile or trace may need to be created
struct CreatePrompt {
    /// Question to be asked to the user
    question: String,

    /// Default reply
    default_reply: &'static str,

    /// Other reply
    other_reply: &'static str,

    /// Default reply means that a new profile should be created
    default_means_create: bool,
}
//
impl CreatePrompt {
    /// Build profile cration prompt when there is no existing build profile
    fn new_build_profile() -> Self {
        Self {
            question: format!(
                "It looks like this build has not been profiled yet. \
                Ready to do so?\n{}",
                Self::BUILD_PROFILE_TRAILER
            ),
            default_reply: "Yes",
            other_reply: "No",
            default_means_create: true,
        }
    }

    /// Build profile cration prompt when the build profile is stale
    fn stale_build_profile() -> Self {
        Self::stale_build_profile_impl(
            "There is an existing build profile, but it is not up to date \
            with respect to current source code. Use it anyway?"
                .to_owned(),
        )
    }

    /// Build profile cration prompt when the build profile might be stale
    ///
    /// If the cpp files changed, we know that the profile is stale, but if
    /// other build dependencies like headers changed, we don't know about it
    /// because CMake won't tell us about those.
    ///
    /// Given that measuring a build profile can take more than an hour, it's
    /// best in any case to ask before overwriting the existing profile, which
    /// may still be good enough.
    ///
    fn maybe_stale_build_profile(age_mins: Option<u64>) -> Self {
        let mut question = "There is an existing build profile".to_owned();
        if let Some(age_mins) = age_mins {
            question.push_str(" from ");
            let age_hours = age_mins / 60;
            let age_days = age_hours / 24;
            if age_days > 0 {
                write!(question, "{age_days} day").expect("Write to String can't fail");
                if age_days > 1 {
                    write!(question, "s").expect("Write to String can't fail");
                }
            } else if age_hours > 0 {
                write!(question, "{age_hours}h").expect("Write to String can't fail");
            } else {
                write!(question, "{age_mins}min").expect("Write to String can't fail");
            }
            question.push_str(" ago");
        }
        question.push_str(". Do you consider it up to date?");
        Self::stale_build_profile_impl(question)
    }

    /// Commonalities between all stale_build_profile functions
    fn stale_build_profile_impl(mut question: String) -> Self {
        question.push('\n');
        question.push_str(Self::BUILD_PROFILE_TRAILER);
        Self {
            question,
            default_reply: "Reuse",
            other_reply: "Measure",
            default_means_create: false,
        }
    }

    /// Common trailer for all build profile creation questions
    const BUILD_PROFILE_TRAILER: &'static str =
        "(Measuring a build profile requires a full single-core build, \
            during which you should minimize other system activity)";

    /// Ask whether a new profile should be created
    fn ask(self, cursive: &mut CursiveRunnable) -> bool {
        let replied_default = Rc::new(Cell::default());
        let reply = |is_default| {
            let replied_default2 = replied_default.clone();
            move |cursive: &mut Cursive| {
                replied_default2.set(is_default);
                cursive.pop_layer();
                cursive.quit();
            }
        };
        cursive.add_layer(
            Dialog::text(self.question)
                .button(self.default_reply, reply(true))
                .button(self.other_reply, reply(false)),
        );
        cursive.run();
        return replied_default.get() ^ !self.default_means_create;
    }
}

/// Measure a build profile, storing the result in a specific location
///
/// Return true if everything worked out, false if that failed and the
/// application must terminate.
///
fn measure_profile(
    cursive: &mut CursiveRunnable,
    path: &Path,
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

    // Start cmakeperf
    let mut collect = match cmakeperf::Collect::start(path) {
        Ok(collect) => collect,
        Err(e) => {
            error(cursive, format!("Failed to start cmakeperf: {e}"));
            return false;
        }
    };

    // Set up progress dialog
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
                            loop {
                                let outcome = match collect.wait_next_step() {
                                    Ok(CollectStep::FileCompiled) => {
                                        counter.tick(1);
                                        continue;
                                    }
                                    Ok(CollectStep::Finished) => Ok(()),
                                    Err(e) => Err(e),
                                };
                                *outcome2.lock().expect("Failed to acquire mutex") = Some(outcome);
                                break;
                            }
                            cb_sink
                                .send(Box::new(|cursive| cursive.quit()))
                                .expect("Failed to exit cursive");
                        }),
                ),
        )
        .button("Abort", Cursive::quit),
    );

    // Measure the profile, handle errors
    let old_fps = cursive.fps();
    cursive.set_autorefresh(true);
    cursive.run();
    cursive.set_fps(old_fps.map(|u| u32::from(u)).unwrap_or(0));
    let error_message_or_abort = match outcome.lock() {
        Ok(mut guard) => match guard.take() {
            Some(Ok(())) => return true,
            Some(Err(e)) => Some(format!("Failed to collect build profile: {e}")),
            None => None,
        },
        Err(e) => Some(format!("Build profile collection thread panicked: {e}")),
    };
    let should_abort = if let Some(message) = error_message_or_abort {
        error(cursive, message);
        false
    } else {
        true
    };

    // Try to delete partially generated files, it does not matter if that
    // cleanup operation fails (it may legitimately do so)
    std::mem::drop(std::fs::remove_file(path));
    if should_abort {
        std::process::abort()
    } else {
        return false;
    }
}

/// Display the full-build profile, let user explore it and analyze traces
fn display_profile(
    cursive: &mut CursiveRunnable,
    compilation_database: CompilationDatabase,
    clangpp: &str,
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

    // Set up table
    let mut table = TableView::<profile::Unit, ProfileColumn>::new()
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

    // Update UI state
    super::with_state(cursive, |state| {
        state.showing_full_build = true;
    });

    // TODO: Add interaction
    // TODO: Use trace::profile where appropriate. Remember to correctly handle
    //       the case where a trace in the build profile isn't present in the
    //       compilation database, which can happen with stale profiles. Also
    //       remember to check for freshness

    // Show the profile
    cursive.add_fullscreen_layer(
        LinearLayout::vertical()
            .child(table.min_size((terminal_width, terminal_height - 1)))
            .child(TextView::new("Press H for help.")),
    );
    cursive.run();
}

/// Truth that a build profile is being displayed
pub fn is_profiling(cursive: &mut Cursive) -> bool {
    super::with_state(cursive, |state| state.showing_full_build)
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
                    .as_secs_f32()
            ),
            ProfileColumn::RelPath(cols) => truncate_path_iter(
                self.rel_path()
                    .as_ref()
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
                .map(|(x, y)| x.cmp(&y))
                .expect("Wall-time should be present if this is probed"),
            ProfileColumn::RelPath(_cols) => {
                self.rel_path().as_ref().cmp(&other.rel_path().as_ref())
            }
        }
    }
}

/// Simple error dialog, to be followed by application exit
fn error(cursive: &mut CursiveRunnable, message: impl Into<String>) {
    cursive.add_layer(Dialog::text(message).button("Quit", Cursive::quit));
    cursive.run();
}
