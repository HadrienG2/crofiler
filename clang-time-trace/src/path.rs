//! Utilities for handling file paths

use lasso::{MiniSpur, Rodeo, RodeoResolver};
use std::{
    collections::HashMap,
    ffi::OsStr,
    ops::Range,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// Space- and allocation-efficient collection of file paths
#[derive(Debug, PartialEq)]
pub struct InternedPaths {
    /// Interned path components
    components: RodeoResolver<ComponentKey>,

    /// Concatened sequence of all interned paths
    paths: Box<[ComponentKey]>,
}
//
impl InternedPaths {
    /// Retrieve a path, panics if the key is invalid
    pub fn get(&self, k: &PathKey) -> InternedPath {
        InternedPath {
            components: &self.components,
            path: &self.paths[k.clone()],
        }
    }
}

/// Key to retrieve an interned path component
///
/// You can use this to compare path components more efficiently than via direct
/// string comparison, as it is guaranteed that if two keys differ, the
/// corresponding values differ as well.
//
// Using MiniSpur here because ~65K source files sounds like it should be
// enough for any sane compilation unit, and the typedef can always be changed
// in the future if I'm proven wrong...
pub type ComponentKey = MiniSpur;

/// Key to retrieve an interned file path
///
/// You can pass this key to ClangTrace::file_path() to access the file path.
///
/// You can also use it to compare paths more efficiently than via direct string
/// comparison, as it is guaranteed that if two keys differ, the corresponding
/// paths differ as well.
pub type PathKey = Range<usize>;

/// Accessor to an interned path
#[derive(Debug, PartialEq)]
pub struct InternedPath<'parent> {
    /// Access to interned path components
    components: &'parent RodeoResolver<ComponentKey>,

    /// Sequence of path components
    path: &'parent [ComponentKey],
}
//
impl<'parent> InternedPath<'parent> {
    /// Iterate over path components
    pub fn components(
        &self,
    ) -> impl Iterator<Item = InternedComponent<'parent>> + DoubleEndedIterator + Clone {
        self.path.iter().map(|&key| InternedComponent {
            key,
            value: self.components.resolve(&key),
        })
    }

    /// Turn the path into a regular Rust filesystem path for convenience
    pub fn to_boxed_path(&self) -> Box<Path> {
        let mut path_buf = PathBuf::with_capacity(self.path.len());
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
    /// Path component interner
    components: Rodeo<ComponentKey>,

    /// Concatenated sequence of all unique paths
    paths: Vec<ComponentKey>,

    /// Path deduplication check
    seen_paths: HashMap<Box<[ComponentKey]>, PathKey>,
}
//
impl PathInterner {
    /// Set up a path interner
    pub fn new() -> Self {
        Self {
            components: Rodeo::new(),
            paths: Vec::new(),
            seen_paths: HashMap::new(),
        }
    }

    /// Record a new file path
    pub fn intern(&mut self, path: impl AsRef<OsStr>) -> Result<PathKey, PathError> {
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
        let path_start = self.paths.len();
        for component in path.components() {
            use std::path::Component::*;
            match component {
                Normal(_) | RootDir | Prefix(_) => {
                    let component_str = component
                        .as_os_str()
                        .to_str()
                        .expect("Since this path comes from JSON, it should be valid Unicode");
                    self.paths
                        .push(self.components.get_or_intern(component_str))
                }
                ParentDir => {
                    // Do not erase root or previous path when resolving parents
                    if self.paths.len() > path_start + 1 {
                        self.paths.pop();
                    }
                }
                CurDir => {}
            }
        }
        let new_path_key = path_start..self.paths.len();

        // If we have seen this path before, then drop it and reuse the key of
        // the existing path, otherwise take note of this newly created path for
        // future deduplication.
        let path_key = self
            .seen_paths
            .entry(self.paths[path_start..].to_vec().into_boxed_slice())
            .and_modify(|_| self.paths.truncate(path_start))
            .or_insert(new_path_key)
            .clone();
        Ok(path_key)
    }

    /// Finalize the collection of paths, keeping all keys valid
    pub fn finalize(self) -> InternedPaths {
        InternedPaths {
            components: self.components.into_resolver(),
            paths: self.paths.into_boxed_slice(),
        }
    }
}

/// What can go wrong when processing a file path
#[derive(Debug, Error, PartialEq)]
pub enum PathError {
    /// Expected absolute file paths, but clang provided a relative one
    ///
    /// This is bad because we do not know the working directory of the clang
    /// process that took the time trace...
    #[error("Expected an absolute file path, got {0:?}")]
    RelativePath(Box<Path>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use more_asserts::*;
    use std::collections::HashSet;

    fn assert_empty(interner: &PathInterner) {
        assert!(interner.components.is_empty());
        assert!(interner.paths.is_empty());
        assert!(interner.seen_paths.is_empty());
    }

    fn extract_components(interner: &PathInterner) -> HashMap<ComponentKey, String> {
        interner
            .components
            .iter()
            .map(|(k, v)| (k, v.to_owned()))
            .collect::<HashMap<_, _>>()
    }

    // Assuming an interner has a good starting state, check that finalization
    // and access to the interned paths works well.
    fn finalize_and_check(interner: PathInterner) {
        // Save interner contents, then finalize interned paths
        let expected_components = extract_components(&interner);
        let expected_paths = interner.paths.clone().into_boxed_slice();
        let path_keys = interner
            .seen_paths
            .values()
            .map(|k| k.clone())
            .collect::<Vec<_>>();
        let interned_paths = interner.finalize();

        // Check components
        assert_eq!(interned_paths.components.len(), expected_components.len());
        for (k, v) in expected_components {
            assert_eq!(interned_paths.components.resolve(&k), v);
        }

        // Check path storage
        assert_eq!(interned_paths.paths, expected_paths);

        // Check path access
        for key in path_keys {
            // Check accessor value
            let interned_path = interned_paths.get(&key);
            assert_eq!(
                interned_path.components as *const _,
                &interned_paths.components as *const _
            );
            let expected_path = &interned_paths.paths[key];
            assert_eq!(interned_path.path, expected_path);

            // Check component access
            let mut expected_path_buf = PathBuf::with_capacity(expected_path.len());
            for (component, expected_key) in interned_path
                .components()
                .zip(expected_path.iter().cloned())
            {
                assert_eq!(
                    component,
                    InternedComponent {
                        key: expected_key,
                        value: interned_paths.components.resolve(&expected_key)
                    }
                );
                assert_eq!(component.key(), component.key);
                assert_eq!(component.value(), component.value);
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
        assert_empty(&interner);
        finalize_and_check(interner);
    }

    #[test]
    fn relative() {
        const PATH: &'static str = "relative";
        let mut interner = PathInterner::new();
        assert_eq!(
            interner.intern(PATH),
            Err(PathError::RelativePath(
                Path::new(PATH).to_path_buf().into_boxed_path()
            ))
        );
        assert_empty(&interner);
        finalize_and_check(interner);
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

        // Check output key
        let expected_key = 0..components.len();
        assert_eq!(key, expected_key);

        // Check path components
        if path == normalized {
            assert_eq!(interner.components.len(), components.len());
        } else {
            assert_ge!(interner.components.len(), components.len());
        }
        assert_eq!(interner.paths.len(), components.len());
        for (key, expected_component) in interner.paths.iter().cloned().zip(components.iter()) {
            let actual_component: &OsStr = interner.components.resolve(&key).as_ref();
            assert_eq!(actual_component, *expected_component);
        }

        // Check seen paths
        assert_eq!(interner.seen_paths.len(), 1);
        assert_eq!(interner.seen_paths[&interner.paths[..]], expected_key);

        // Check interner finalization
        finalize_and_check(interner);
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
        let old_paths = interner.paths.clone();
        let old_seen_paths = interner.seen_paths.clone();

        // Intern the second path
        let key2 = interner.intern(path2).unwrap();

        // Were they identical ?
        if path1 == path2 {
            // If so, the interner should stay the same and return the same key
            assert_eq!(key2, key1);
            assert_eq!(extract_components(&interner), old_components);
            assert_eq!(interner.paths, old_paths);
            assert_eq!(interner.seen_paths, old_seen_paths);
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
            assert_eq!(interner.paths.len(), old_paths.len() + components2.len());
            let old_path = &interner.paths[..old_paths.len()];
            assert_eq!(old_path, old_paths);
            let new_path = &interner.paths[old_paths.len()..];
            for (key, &expected_component) in new_path.iter().zip(components2.iter()) {
                let actual_component: &OsStr = interner.components.resolve(key).as_ref();
                assert_eq!(actual_component, expected_component);
            }

            // The new path is added to the deduplication record
            assert_eq!(interner.seen_paths.len(), 2);
            assert_eq!(interner.seen_paths[old_path], key1);
            assert_eq!(interner.seen_paths[new_path], key2);
        }

        // Check interner finalization
        finalize_and_check(interner);
    }

    #[test]
    fn dual() {
        test_dual_path("/a/b.c", "/a/b.c");
        test_dual_path("/a/b.c", "/a/d.e");
        test_dual_path("/a/b.c", "/d/b.c");
        test_dual_path("/a/b.c", "/d/e.f");
    }
}
