//! Utilities for handling file paths

use crate::{
    sequence::{InternedSequences, SequenceInterner, SequenceKey},
    InternerKey,
};
use lasso::{Key, Resolver, Rodeo, RodeoResolver, Spur};
use std::{
    ffi::OsStr,
    fmt::{self, Display, Formatter},
    hash::Hash,
    ops::Range,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// Object that contains interned file paths
pub trait PathResolver {
    /// Interning key that uniquely identifies a file path
    type PathKey: InternerKey<ImplKey = Range<usize>>;

    /// Interning key that uniquely identifies a path component
    type ComponentKey: Key;

    /// Resolver used to get to path components
    #[doc(hidden)]
    type ComponentResolver: Resolver<Self::ComponentKey>;

    /// Access the internal component resolver
    #[doc(hidden)]
    fn component_resolver(&self) -> &Self::ComponentResolver;

    /// Retrieve a set of path components
    #[doc(hidden)]
    fn get_path_components(&self, key: Self::PathKey) -> &[Self::ComponentKey];

    /// Retrieve an object using its interning key
    fn get(&self, key: Self::PathKey) -> InternedPath<Self> {
        InternedPath::new(self, key)
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
pub type PathKey<KeyImpl = Spur, const LEN_BITS: u32 = 8> = SequenceKey<KeyImpl, LEN_BITS>;

/// Space- and allocation-efficient collection of file paths
#[derive(Debug, Eq, PartialEq)]
pub struct InternedPaths<
    ComponentKey: Key,
    PK: InternerKey<ImplKey = Range<usize>> = PathKey<Spur, 8>,
> {
    /// Interned path components
    components: RodeoResolver<ComponentKey>,

    /// Concatened sequence of all interned paths
    sequences: InternedSequences<ComponentKey, PK>,
}
//
impl<ComponentKey: Key, PK: InternerKey<ImplKey = Range<usize>>> InternedPaths<ComponentKey, PK> {
    /// Retrieve a path, panics if the key is invalid
    pub fn get(&self, key: PK) -> InternedPath<Self> {
        <Self as PathResolver>::get(self, key)
    }
}
//
impl<ComponentKey: Key, PK: InternerKey<ImplKey = Range<usize>>> PathResolver
    for InternedPaths<ComponentKey, PK>
{
    type PathKey = PK;
    type ComponentKey = ComponentKey;
    type ComponentResolver = RodeoResolver<ComponentKey>;

    fn component_resolver(&self) -> &Self::ComponentResolver {
        &self.components
    }

    fn get_path_components(&self, key: PK) -> &[Self::ComponentKey] {
        self.sequences.get(key)
    }
}

/// Accessor to an interned path
#[derive(Debug)]
pub struct InternedPath<'parent, Parent: PathResolver + ?Sized> {
    #[cfg(not(feature = "reffers"))]
    /// Parent from which this path comes
    parent: &'parent Parent,

    #[cfg(feature = "reffers")]
    /// Parent from which this path comes
    parent: reffers::ARef<'parent, Parent>,

    /// Interned path key
    key: Parent::PathKey,
}
//
impl<'parent, Parent: PathResolver + ?Sized> InternedPath<'parent, Parent> {
    #[cfg(not(feature = "reffers"))]
    /// Get access to an interned path
    pub fn new(parent: &'parent Parent, key: Parent::PathKey) -> Self {
        Self { parent, key }
    }

    #[cfg(feature = "reffers")]
    /// Get access to an interned path
    pub fn new(parent: impl Into<reffers::ARef<'parent, Parent>>, key: Parent::PathKey) -> Self {
        Self {
            parent: parent.into(),
            key,
        }
    }

    /// Iterate over path components
    pub fn components<'self_>(
        &'self_ self,
    ) -> impl Iterator<Item = InternedComponent<'self_, Parent::ComponentKey>>
           + DoubleEndedIterator
           + Clone
           + 'self_
           + Captures<'parent> {
        self.parent
            .get_path_components(self.key)
            .iter()
            .map(move |&key| InternedComponent {
                key,
                value: self.parent.component_resolver().resolve(&key),
            })
    }

    /// Turn the path into a regular Rust filesystem path for convenience
    pub fn to_boxed_path(&self) -> Box<Path> {
        let mut path_buf = PathBuf::new();
        for component in self.components() {
            path_buf.push(component);
        }
        path_buf.into_boxed_path()
    }
}
//
impl<'parent, Parent: PathResolver + ?Sized> PartialEq for InternedPath<'parent, Parent> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && std::ptr::eq(&*self.parent, &*other.parent)
    }
}
//
/// Workaround for impl Trait limitation
pub trait Captures<'a> {}
impl<T: ?Sized> Captures<'_> for T {}
//
impl<'parent, Parent: PathResolver + ?Sized> Display for InternedPath<'parent, Parent> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.to_boxed_path().display())
    }
}

/// Accessor to an interned path component
#[derive(Debug, Eq, PartialEq)]
pub struct InternedComponent<'parent, ComponentKey: Key = Spur> {
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

/// Writable collection of absolute file paths, meant to ultimately become InternedPaths
pub struct PathInterner<
    ComponentKey: Key + Hash,
    PK: InternerKey<ImplKey = Range<usize>> = PathKey<Spur, 8>,
> {
    /// Interner for individual path components
    components: Rodeo<ComponentKey>,

    /// Interner for the sequences of interned components that make up a path
    sequences: SequenceInterner<ComponentKey, PK>,
}
//
impl<ComponentKey: Key + Hash, PK: InternerKey<ImplKey = Range<usize>>>
    PathInterner<ComponentKey, PK>
{
    /// Set up a path interner
    pub fn new() -> Self {
        Self {
            components: Rodeo::new(),
            sequences: SequenceInterner::new(),
        }
    }

    /// Record a new absolute file path, return an error if the path is relative
    ///
    /// Basic path normalization is performed, handling . and .. as you would
    /// expect, but symlinks are not traversed as access to the filesystem tree
    /// where the data was taken cannot be assumed.
    ///
    /// Paths must all be absolute as otherwise, without working directory
    /// information, there is no way to compare a relative path with another path
    ///
    pub fn intern(&mut self, path: &str) -> Result<PK, PathError> {
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
        let mut sequence = self.sequences.entry();
        for component in path.components() {
            use std::path::Component::*;
            match component {
                Normal(_) | RootDir | Prefix(_) => {
                    let component_str = component
                        .as_os_str()
                        .to_str()
                        .expect("Since this path comes from an &str, it must be valid Unicode");
                    sequence.push(self.components.get_or_intern(component_str))
                }
                ParentDir => {
                    // Do not erase root when resolving parents
                    if sequence.len() > 1 {
                        sequence.pop();
                    }
                }
                CurDir => {}
            }
        }

        // Intern the sequence that makes up the path
        Ok(sequence.intern())
    }

    /// Retrieve a previously interned path
    pub fn get(&self, key: PK) -> InternedPath<Self> {
        <Self as PathResolver>::get(self, key)
    }

    /// Truth that no path has been interned yet
    pub fn is_empty(&self) -> bool {
        self.sequences.is_empty()
    }

    /// Number of interned paths
    pub fn len(&self) -> usize {
        self.sequences.len()
    }

    /// Total number of path components across all interned paths
    pub fn num_components(&self) -> usize {
        self.sequences.num_items()
    }

    /// Total number of unique (interned) path components
    pub fn num_unique_components(&self) -> usize {
        self.components.len()
    }

    /// Maximal path length
    pub fn max_path_len(&self) -> Option<usize> {
        self.sequences.max_sequence_len()
    }

    /// Finalize the collection of paths, keeping all keys valid
    pub fn finalize(self) -> InternedPaths<ComponentKey, PK> {
        InternedPaths {
            components: self.components.into_resolver(),
            sequences: self.sequences.finalize(),
        }
    }
}
//
impl<ComponentKey: Key + Hash, PK: InternerKey<ImplKey = Range<usize>>> Default
    for PathInterner<ComponentKey, PK>
{
    fn default() -> Self {
        Self::new()
    }
}
//
impl<ComponentKey: Key + Hash, PK: InternerKey<ImplKey = Range<usize>>> PathResolver
    for PathInterner<ComponentKey, PK>
{
    type PathKey = PK;
    type ComponentKey = ComponentKey;
    type ComponentResolver = Rodeo<ComponentKey>;

    fn component_resolver(&self) -> &Self::ComponentResolver {
        &self.components
    }

    fn get_path_components(&self, key: PK) -> &[Self::ComponentKey] {
        self.sequences.get(key)
    }
}

/// What can go wrong when processing a file path
#[derive(Debug, Eq, Error, PartialEq)]
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
    type TestedInterner = PathInterner<TestedComponentKey, PathKey>;

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
                interned_path.components().count(),
                expected_components.len()
            );

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

        assert_eq!(interner.num_components(), 0);
        assert_eq!(interner.num_unique_components(), 0);
        assert_eq!(interner.max_path_len(), None);
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());

        finalize_and_check(interner, &[]);
    }

    fn test_single_abs_path(path: &str, normalized: &str) {
        // Determine expected normalized components
        let components = Path::new(normalized)
            .components()
            .map(|c| c.as_os_str())
            .collect::<Vec<_>>();

        // Intern the path
        let mut interner = TestedInterner::new();
        let key = interner
            .intern(path)
            .expect("This function should only be used to test interning of absolute paths");

        // Check basic interner state
        if path == normalized {
            assert_eq!(interner.num_unique_components(), components.len());
        } else {
            assert_ge!(interner.num_unique_components(), components.len());
        }
        assert_eq!(interner.sequences.num_items(), components.len());
        assert_eq!(interner.sequences.len(), 1);

        // Check accessors
        assert_eq!(interner.num_components(), interner.sequences.num_items());
        assert_eq!(interner.num_unique_components(), interner.components.len());
        assert_eq!(interner.max_path_len(), Some(components.len()));
        assert_eq!(interner.len(), 1);
        assert!(!interner.is_empty());

        // Check final sequence
        finalize_and_check(interner, &[(components, key)]);
    }

    #[test]
    fn single() {
        // Check that relative paths error out
        let mut interner = TestedInterner::new();
        let rel_path = "./relative";
        assert_eq!(
            interner.intern(rel_path),
            Err(PathError::RelativePath(Path::new(rel_path).into()))
        );

        // Check that absolute paths work well
        test_single_abs_path("/a/b.c", "/a/b.c");
        test_single_abs_path("/./a/b.c", "/a/b.c");
        test_single_abs_path("/../a/b.c", "/a/b.c");
        test_single_abs_path("/a/./b.c", "/a/b.c");
        test_single_abs_path("/a/../b.c", "/b.c");
    }

    fn test_dual_abs_path(path1: &str, path2: &str) {
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
        let key1 = interner
            .intern(path1)
            .expect("This function should only be used to test interning of absolute paths");

        // Back up the interner state
        let old_components = extract_components(&interner);
        let old_sequences = interner.sequences.clone();

        // Intern the second path
        let key2 = interner
            .intern(path2)
            .expect("This function should only be used to test interning of absolute paths");

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
            assert_eq!(interner.num_unique_components(), (&set2 | &set1).len());

            // Old path components remain available with the same key
            for (k, v) in old_components {
                assert_eq!(interner.components.resolve(&k), v);
            }

            // New path components are available with new keys
            for component in &set2 - &set1 {
                assert!(interner.components.contains(
                    component
                        .to_str()
                        .expect("Paths come from strings, so they should be UTF-8")
                ));
            }

            // Old path is unchanged, new path is added using new components
            assert_eq!(interner.sequences.len(), 2);
            assert_eq!(
                interner.sequences.num_items(),
                old_sequences.num_items() + components2.len()
            );

            // Check accessors
            assert_eq!(interner.num_components(), interner.sequences.num_items());
            assert_eq!(interner.num_unique_components(), interner.components.len());
            assert_eq!(
                interner.max_path_len(),
                Some(components1.len().max(components2.len()))
            );
            assert_eq!(interner.len(), 2);
            assert!(!interner.is_empty());
        }

        // Check interner finalization
        finalize_and_check(interner, &[(components1, key1), (components2, key2)]);
    }

    #[test]
    fn dual() {
        test_dual_abs_path("/a/b.c", "/a/b.c");
        test_dual_abs_path("/a/b.c", "/a/d.e");
        test_dual_abs_path("/a/b.c", "/d/b.c");
        test_dual_abs_path("/a/b.c", "/d/e.f");
    }
}
