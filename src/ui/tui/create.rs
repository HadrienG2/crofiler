//! Prompt before creating a build profile (either full-build or single-file)

use cmakeperf::commands::ProductFreshness;
use cursive::{views::Dialog, Cursive, CursiveRunnable};
use std::{cell::Cell, fmt::Write, rc::Rc};

/// Prompt to be shown when a new profile or trace may need to be created
pub struct CreatePrompt {
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
    /// Given a profiling product's freshness, emit a (re)creation prompt if needed
    ///
    /// - manually_specified indicates that a profile was manually specified by
    ///   the user, which disabled "maybe stale" dialogs.
    ///
    pub fn from_freshness(freshness: ProductFreshness, manually_specified: bool) -> Option<Self> {
        match freshness {
            // No profile at the expected location
            ProductFreshness::Nonexistent => Some(CreatePrompt::new_build_profile()),

            // There is a profile but it is obviously stale
            ProductFreshness::Outdated => Some(CreatePrompt::stale_build_profile()),

            // There is a profile, and it does not look obviously stale, but
            // might still be because we are not omniscient. Prompt if it's older
            // than one minute and we're using the default path (not manual choice)
            ProductFreshness::MaybeOutdated(age) => {
                let age_mins = age.map(|d| d.as_secs() / 60).unwrap_or(u64::MAX);
                if age_mins > 0 && !manually_specified {
                    Some(CreatePrompt::maybe_stale_build_profile(Some(age_mins)))
                } else {
                    None
                }
            }
        }
    }

    /// Show the dialog, asking whether a new profile should be created
    pub fn show(
        self,
        cursive: &mut Cursive,
        handle_reply: impl FnOnce(&mut Cursive, bool) + 'static,
    ) {
        let handle_reply = Cell::new(Some(handle_reply));
        let handle_reply = Rc::new(move |cursive: &mut Cursive, should_create| {
            cursive.pop_layer();
            let handle_reply = handle_reply.take().expect("This can only be called once");
            handle_reply(cursive, should_create)
        });
        let handle_reply_2 = handle_reply.clone();
        cursive.add_layer(
            Dialog::text(self.question)
                .button(self.default_reply, move |cursive| {
                    handle_reply(cursive, self.default_means_create)
                })
                .button(self.other_reply, move |cursive| {
                    handle_reply_2(cursive, !self.default_means_create)
                }),
        );
    }

    /// Same, but wait for answer and return it (requires CursiveRunnable)
    ///
    /// `None` can be returned if users activate the Quit command without
    /// answering the question, in which case the caller should terminate.
    ///
    pub fn ask(self, cursive: &mut CursiveRunnable) -> Option<bool> {
        let should_create = Rc::new(Cell::default());
        let should_create_2 = should_create.clone();
        self.show(cursive, move |cursive, should_create| {
            should_create_2.set(Some(should_create));
            cursive.quit();
        });
        cursive.run();
        should_create.get()
    }

    /// Build profile cration prompt when there is no existing build profile
    fn new_build_profile() -> Self {
        Self {
            question: "It looks like this build has not been profiled yet. \
                Ready to do so?"
                .to_owned(),
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
    fn stale_build_profile_impl(question: String) -> Self {
        Self {
            question,
            default_reply: "Reuse",
            other_reply: "Measure",
            default_means_create: false,
        }
    }
}
