//! Utilities for handling file paths

use crate::sequence::{InternedSequences, SequenceInterner, SequenceKey};
use lasso::{Key, Rodeo, RodeoResolver};
use std::{
    ffi::OsStr,
    hash::Hash,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// Space- and allocation-efficient collection of file paths
#[derive(Debug, PartialEq)]
pub struct InternedPaths<ComponentKey: Key, PathKeyImpl: Key, const LEN_BITS: u32> {
    /// Interned path components
    components: RodeoResolver<ComponentKey>,

    /// Concatened sequence of all interned paths
    sequences: InternedSequences<ComponentKey, PathKeyImpl, LEN_BITS>,
}
//
impl<ComponentKey: Key, PathKeyImpl: Key, const LEN_BITS: u32>
    InternedPaths<ComponentKey, PathKeyImpl, LEN_BITS>
{
    /// Retrieve a path, panics if the key is invalid
    pub fn get(&self, key: PathKey<PathKeyImpl, LEN_BITS>) -> InternedPath<ComponentKey> {
        InternedPath {
            components: &self.components,
            sequence: self.sequences.get(key),
        }
    }
}

/// Key to retrieve an interned file path
///
/// You can pass this key to InternedPaths::get() to access the file path.
///
/// You can also use it to compare paths more efficiently than via direct string
/// comparison, as it is guaranteed that if two keys differ, the corresponding
/// paths differ as well.
///
pub type PathKey<KeyImpl, const LEN_BITS: u32> = SequenceKey<KeyImpl, LEN_BITS>;

/// Accessor to an interned path
#[derive(Debug, PartialEq)]
pub struct InternedPath<'parent, ComponentKey: Key> {
    /// Access to interned path components
    components: &'parent RodeoResolver<ComponentKey>,

    /// Sequence of path components
    sequence: &'parent [ComponentKey],
}
//
impl<'parent, ComponentKey: Key> InternedPath<'parent, ComponentKey> {
    /// Iterate over path components
    pub fn components(
        &self,
    ) -> impl Iterator<Item = InternedComponent<'parent, ComponentKey>> + DoubleEndedIterator + Clone
    {
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
pub struct InternedComponent<'parent, ComponentKey: Key> {
    /// Key used to extract the path component
    key: ComponentKey,

    /// Path component
    value: &'parent str,
}
//
impl<'parent, ComponentKey: Key> InternedComponent<'parent, ComponentKey> {
    /// Interned path component
    pub fn value(&self) -> &'parent str {
        self.value
    }

    /// Key used to intern the path component
    ///
    /// Keys are cheaper to compare than path components, and it is guaranteed
    /// that two path components are equal if an only if a.key() == b.key().
    ///
    pub fn key(&self) -> ComponentKey {
        self.key
    }
}
//
impl<ComponentKey: Key> AsRef<str> for InternedComponent<'_, ComponentKey> {
    fn as_ref(&self) -> &str {
        self.value()
    }
}
//
impl<ComponentKey: Key> AsRef<OsStr> for InternedComponent<'_, ComponentKey> {
    fn as_ref(&self) -> &OsStr {
        self.value().as_ref()
    }
}
//
impl<ComponentKey: Key> AsRef<Path> for InternedComponent<'_, ComponentKey> {
    fn as_ref(&self) -> &Path {
        self.value().as_ref()
    }
}

/// Writable collection of file paths, meant to ultimately become InternedPaths
pub struct PathInterner<ComponentKey: Key + Hash, PathKeyImpl: Key, const LEN_BITS: u32> {
    /// Interner for individual path components
    components: Rodeo<ComponentKey>,

    /// Interner for the sequences of interned components that make up a path
    sequences: SequenceInterner<ComponentKey, PathKeyImpl, LEN_BITS>,

    /// Cached allocation for the current sequence
    current_sequence: Vec<ComponentKey>,
}
//
impl<ComponentKey: Key + Hash, PathKeyImpl: Key, const LEN_BITS: u32>
    PathInterner<ComponentKey, PathKeyImpl, LEN_BITS>
{
    /// Set up a path interner
    pub fn new() -> Self {
        Self {
            components: Rodeo::new(),
            sequences: SequenceInterner::new(),
            current_sequence: Vec::new(),
        }
    }

    /// Record a new file path, return an error if the path is relative
    pub fn intern(&mut self, path: &str) -> Result<PathKey<PathKeyImpl, LEN_BITS>, PathError> {
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

        // Intern the sequence that makes up the path
        Ok(self.sequences.intern(&self.current_sequence[..]))
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

    /// Query maximal inner sequence length
    pub fn max_path_len(&self) -> Option<usize> {
        self.sequences.max_sequence_len()
    }

    /// Finalize the collection of paths, keeping all keys valid
    pub fn finalize(self) -> InternedPaths<ComponentKey, PathKeyImpl, LEN_BITS> {
        InternedPaths {
            components: self.components.into_resolver(),
            sequences: self.sequences.finalize(),
        }
    }
}
//
impl<ComponentKey: Key + Hash, PathKeyImpl: Key, const LEN_BITS: u32> Default
    for PathInterner<ComponentKey, PathKeyImpl, LEN_BITS>
{
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
    use lasso::{MiniSpur, Spur};
    use more_asserts::*;
    use pretty_assertions::{assert_eq, assert_ne};
    use std::collections::{HashMap, HashSet};

    type TestedComponentKey = MiniSpur;
    type TestedPathKey = PathKey<Spur, 8>;
    type TestedInterner = PathInterner<TestedComponentKey, Spur, 8>;

    fn extract_components(interner: &TestedInterner) -> HashMap<TestedComponentKey, String> {
        interner
            .components
            .iter()
            .map(|(k, v)| (k, v.to_owned()))
            .collect::<HashMap<_, _>>()
    }

    // Assuming an interner has a good starting state, check that finalization
    // and access to the interned paths works well.
    fn finalize_and_check(interner: TestedInterner, inputs: &[(Vec<&OsStr>, TestedPathKey)]) {
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
        let interner = TestedInterner::new();
        assert!(interner.components.is_empty());
        assert!(interner.sequences.is_empty());
        finalize_and_check(interner, &[]);
    }

    fn test_single_path(path: &str, normalized: &str) {
        // Determine expected normalized components
        let components = Path::new(normalized)
            .components()
            .map(|c| c.as_os_str())
            .collect::<Vec<_>>();

        // Intern the path
        let mut interner = TestedInterner::new();
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
        let mut interner = TestedInterner::new();
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
            assert_eq!(
                interner.sequences.clone().finalize(),
                old_sequences.clone().finalize()
            );
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
