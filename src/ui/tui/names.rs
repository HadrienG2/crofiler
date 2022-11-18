//! Cursive view names
//!
//! At this point in time, cursive can't detect duplicate view names, so we need
//! a centralized view name repository to find them in tests.

/// Name of a cursive view
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ViewName {
    /// View name shared by all global dialogs (help, quit, backtrace...)
    GlobalDialog = 0,

    /// Full-build profile view name
    FullBuildProfile = 1,

    /// Prefix of activity profile table view name
    ActivityTablePrefix = 2,

    /// Prefix of activity profile description view name
    ActivityDescPrefix = 3,
}
//
impl AsRef<str> for ViewName {
    fn as_ref(&self) -> &str {
        VIEW_NAMES[*self as usize]
    }
}

/// Strings associated with each ViewName
const VIEW_NAMES: &[&str] = &[
    "Global dialog",
    "Full-build profile",
    "Activity table #",
    "Activity description #",
];

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn no_collision() {
        let set = VIEW_NAMES.iter().collect::<HashSet<_>>();
        assert_eq!(set.len(), VIEW_NAMES.len());
    }
}
