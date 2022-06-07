//! Utilities for handling file paths

use crate::sequence::{InternedSequences, SequenceInterner};
use lasso::{MiniSpur, Rodeo, RodeoResolver};
use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// Space- and allocation-efficient collection of file paths
#[derive(Debug, PartialEq)]
pub struct InternedPaths {
    /// Interned path components
    components: RodeoResolver<ComponentKey>,

    /// Concatened sequence of all interned paths
    sequences: InternedSequences<ComponentKey>,
}
//
impl InternedPaths {
    /// Retrieve a path, panics if the key is invalid
    pub fn get(&self, key: PathKey) -> InternedPath {
        let base = (key & (2u32.pow(24) - 1)) as usize;
        let len = (key >> 24) as usize;
        InternedPath {
            components: &self.components,
            sequence: self.sequences.get(base..base + len),
        }
    }
}

/// Key to retrieve an interned path component
///
/// You can use this to compare path components more efficiently than via direct
/// string comparison, as it is guaranteed that if two keys differ, the
/// corresponding values differ as well.
//
// Using MiniSpur here because for what this crate is designed to do
// (compilation profile analysis), I'm expecting no more than 2^16 unique path
// components. If there is demand, I can late make this generic.
//
pub type ComponentKey = MiniSpur;

/// Key to retrieve an interned file path
///
/// You can pass this key to InternedPaths::get() to access the file path.
///
/// You can also use it to compare paths more efficiently than via direct string
/// comparison, as it is guaranteed that if two keys differ, the corresponding
/// paths differ as well.
//
// This is actually a bit-packed SequenceKey. The 8 high-order bits represent
// the length of the sequence and the 24 low-order bits represent its base.
// This should work because I'm expecting no more than 2^16 paths, each no
// longer than 2^8 components long. So the concatenation will be no more than
// 2^16 * 2^8 = 2^24 components, making the base fit in 24 bits. Adding a 8-bit
// length to that, everything should fit in 24 + 8 = 32 bits.
//
pub type PathKey = u32;

/// Accessor to an interned path
#[derive(Debug, PartialEq)]
pub struct InternedPath<'parent> {
    /// Access to interned path components
    components: &'parent RodeoResolver<ComponentKey>,

    /// Sequence of path components
    sequence: &'parent [ComponentKey],
}
//
impl<'parent> InternedPath<'parent> {
    /// Iterate over path components
    pub fn components(
        &self,
    ) -> impl Iterator<Item = InternedComponent<'parent>> + DoubleEndedIterator + Clone {
        self.sequence.iter().map(|&key| InternedComponent {
            key,
            value: self.components.resolve(&key),
        })
    }

    /// Turn the path into a regular Rust filesystem path for convenience
    pub fn to_boxed_path(&self) -> Box<Path> {
        let mut path_buf = PathBuf::with_capacity(self.sequence.len());
        for component in self.components() {
            path_buf.push(component);
        }
        path_buf.into_boxed_path()
    }
}

/// Accessor to an interned path component
#[derive(Debug, PartialEq)]
pub struct InternedComponent<'parent> {
    /// Key used to extract the path component
    key: ComponentKey,

    /// Path component
    value: &'parent str,
}
//
impl<'parent> InternedComponent<'parent> {
    /// Interned path component
    pub fn value(&self) -> &'parent str {
        self.value
    }

    /// Key used to intern the path component
    ///
    /// Keys are cheaper to compare than path components, and it is guaranteed
    /// that two path components are equal if an only if a.key() == b.key().
    pub fn key(&self) -> ComponentKey {
        self.key
    }
}
//
impl AsRef<str> for InternedComponent<'_> {
    fn as_ref(&self) -> &str {
        self.value()
    }
}
//
impl AsRef<OsStr> for InternedComponent<'_> {
    fn as_ref(&self) -> &OsStr {
        self.value().as_ref()
    }
}
//
impl AsRef<Path> for InternedComponent<'_> {
    fn as_ref(&self) -> &Path {
        self.value().as_ref()
    }
}

/// Writable collection of file paths, meant to ultimately become InternedPaths
#[derive(Debug, PartialEq)]
pub struct PathInterner {
    /// Interner for individual path components
    components: Rodeo<ComponentKey>,

    /// Interner for the sequences of interned components that make up a path
    sequences: SequenceInterner<ComponentKey>,

    /// Cached allocation for the current sequence
    current_sequence: Vec<ComponentKey>,
}
//
impl PathInterner {
    /// Set up a path interner
    pub fn new() -> Self {
        Self {
            components: Rodeo::new(),
            sequences: SequenceInterner::new(),
            current_sequence: Vec::new(),
        }
    }

    /// Record a new file path, return None if the path is not Unicode
    pub fn intern(&mut self, path: &str) -> Result<PathKey, PathError> {
        // Parse input string as a filesystem path
        let path = Path::new(&path);

        // Make sure the path is absolute
        if path.is_relative() {
            return Err(PathError::RelativePath(
                path.to_path_buf().into_boxed_path(),
            ));
        }

        // Turn the path into a form that is as normalized as possible without
        // having access to the filesystem on which the clang trace was taken:
        // can process . and .. sequences, but not follow symlinks.
        self.current_sequence.clear();
        for component in path.components() {
            use std::path::Component::*;
            match component {
                Normal(_) | RootDir | Prefix(_) => {
                    let component_str = component
                        .as_os_str()
                        .to_str()
                        .expect("Since this path comes from an &str, it must be valid Unicode");
                    self.current_sequence
                        .push(self.components.get_or_intern(component_str))
                }
                ParentDir => {
                    // Do not erase root when resolving parents
                    if self.current_sequence.len() > 1 {
                        self.current_sequence.pop();
                    }
                }
                CurDir => {}
            }
        }

        // Intern the sequence that makes up the path, and compress the key
        // according to our expectations.
        let sequence_key = self.sequences.intern(&self.current_sequence[..]);
        assert!(
            sequence_key.start < 2usize.pow(24),
            "Unexpected number of interned path components"
        );
        let path_len = sequence_key.end - sequence_key.start;
        assert!(path_len < 2usize.pow(8), "Unexpected path length");
        let key = sequence_key.start as u32 | ((path_len as u32) << 24);
        Ok(key)
    }

    /// Truth that no path has been interned yet
    pub fn is_empty(&self) -> bool {
        self.sequences.is_empty()
    }

    /// Query number of interned paths
    pub fn len(&self) -> usize {
        self.sequences.len()
    }

    /// Query total number of interned components across all interned paths
    pub fn num_components(&self) -> usize {
        self.sequences.num_items()
    }

    /// Finalize the collection of paths, keeping all keys valid
    pub fn finalize(self) -> InternedPaths {
        InternedPaths {
            components: self.components.into_resolver(),
            sequences: self.sequences.finalize(),
        }
    }
}
//
impl Default for PathInterner {
    fn default() -> Self {
        Self::new()
    }
}

/// What can go wrong when processing a file path
#[derive(Debug, Error, PartialEq)]
pub enum PathError {
    /// Expected absolute file paths, but clang provided a relative one
    ///
    /// This is bad because we do not know the working directory of the clang
    /// process that took the time trace...
    ///
    #[error("Expected an absolute file path, got {0:?}")]
    RelativePath(Box<Path>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use more_asserts::*;
    use pretty_assertions::{assert_eq, assert_ne};
    use std::collections::{HashMap, HashSet};

    fn extract_components(interner: &PathInterner) -> HashMap<ComponentKey, String> {
        interner
            .components
            .iter()
            .map(|(k, v)| (k, v.to_owned()))
            .collect::<HashMap<_, _>>()
    }

    // Assuming an interner has a good starting state, check that finalization
    // and access to the interned paths works well.
    fn finalize_and_check(interner: PathInterner, inputs: &[(Vec<&OsStr>, PathKey)]) {
        // Save interner contents, then finalize interned paths
        let expected_components = extract_components(&interner);
        let interned_paths = interner.finalize();

        // Check components
        assert_eq!(interned_paths.components.len(), expected_components.len());
        for (k, v) in expected_components {
            assert_eq!(interned_paths.components.resolve(&k), v);
        }

        // Check path access
        for (expected_components, key) in inputs {
            // Check accessor value
            let interned_path = interned_paths.get(*key);
            assert_eq!(
                interned_path.components as *const _,
                &interned_paths.components as *const _
            );
            assert_eq!(interned_path.sequence.len(), expected_components.len());

            // Check component access
            let mut expected_path_buf = PathBuf::with_capacity(expected_components.len());
            for (component, expected_component) in
                interned_path.components().zip(expected_components)
            {
                let value: &OsStr = component.value().as_ref();
                assert_eq!(&value, expected_component);
                expected_path_buf.push(component.value());
            }

            // Check path construction
            assert_eq!(
                interned_path.to_boxed_path(),
                expected_path_buf.into_boxed_path()
            );
        }
    }

    #[test]
    fn empty() {
        let interner = PathInterner::new();
        assert!(interner.components.is_empty());
        assert_eq!(interner.sequences, SequenceInterner::new());
        finalize_and_check(interner, &[]);
    }

    fn test_single_path(path: &str, normalized: &str) {
        // Determine expected normalized components
        let components = Path::new(normalized)
            .components()
            .map(|c| c.as_os_str())
            .collect::<Vec<_>>();

        // Intern the path
        let mut interner = PathInterner::new();
        let key = interner.intern(path).unwrap();

        // Check basic interner state
        if path == normalized {
            assert_eq!(interner.components.len(), components.len());
        } else {
            assert_ge!(interner.components.len(), components.len());
        }
        assert_eq!(interner.sequences.num_items(), components.len());
        assert_eq!(interner.sequences.len(), 1);

        // Check final sequence
        finalize_and_check(interner, &[(components, key)]);
    }

    #[test]
    fn single() {
        test_single_path("/a/b.c", "/a/b.c");
        test_single_path("/./a/b.c", "/a/b.c");
        test_single_path("/../a/b.c", "/a/b.c");
        test_single_path("/a/./b.c", "/a/b.c");
        test_single_path("/a/../b.c", "/b.c");
    }

    fn test_dual_path(path1: &str, path2: &str) {
        // Determine expected components
        let components = |path| {
            Path::new(path)
                .components()
                .map(|c| c.as_os_str())
                .collect::<Vec<_>>()
        };
        let components1 = components(path1);
        let components2 = components(path2);

        // Intern the first path
        let mut interner = PathInterner::new();
        let key1 = interner.intern(path1).unwrap();

        // Back up the interner state
        let old_components = extract_components(&interner);
        let old_sequences = interner.sequences.clone();

        // Intern the second path
        let key2 = interner.intern(path2).unwrap();

        // Were they identical ?
        if path1 == path2 {
            // If so, the interner should stay the same and return the same key
            assert_eq!(key2, key1);
            assert_eq!(extract_components(&interner), old_components);
            assert_eq!(interner.sequences, old_sequences);
        } else {
            // Otherwise, different keys are returned and the state changes
            assert_ne!(key2, key1);

            // The set of path components is extended to encompass the union of
            // path components associated with both paths
            let set1 = components1.iter().cloned().collect::<HashSet<&OsStr>>();
            let set2 = components2.iter().cloned().collect::<HashSet<&OsStr>>();
            assert_eq!(interner.components.len(), (&set2 | &set1).len());

            // Old path components remain available with the same key
            for (k, v) in old_components {
                assert_eq!(interner.components.resolve(&k), v);
            }

            // New path components are available with new keys
            for component in &set2 - &set1 {
                assert!(interner.components.contains(component.to_str().unwrap()));
            }

            // Old path is unchanged, new path is added using new components
            assert_eq!(interner.sequences.len(), 2);
            assert_eq!(
                interner.sequences.num_items(),
                old_sequences.num_items() + components2.len()
            );
        }

        // Check interner finalization
        finalize_and_check(interner, &[(components1, key1), (components2, key2)]);
    }

    #[test]
    fn dual() {
        test_dual_path("/a/b.c", "/a/b.c");
        test_dual_path("/a/b.c", "/a/d.e");
        test_dual_path("/a/b.c", "/d/b.c");
        test_dual_path("/a/b.c", "/d/e.f");
    }
}
