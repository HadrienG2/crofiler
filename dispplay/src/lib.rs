mod entities;

use lasso::{MiniSpur, Rodeo};
use std::{fmt::Write, num::NonZeroU8};

// Goal: Hierarchical display structure that can be used for pretty-printing and
//       simplification
//
// Process: Make it work, then make it fast

/// State to be kept around while rendering multiple entities
#[derive(Debug, Eq, PartialEq)]
pub struct Context {
    /// Interner for tree node identifiers
    ids: Rodeo<Id>,

    /// Cache of previously allocated leaf strings
    leaf_cache: Vec<String>,

    /// Cache of vectors of children for Tree::List
    list_cache: Vec<Vec<Tree>>,
}
//
impl Context {
    /// Set up a rendering context
    pub fn new() -> Self {
        Self {
            ids: Rodeo::new(),
            leaf_cache: Vec::new(),
            list_cache: Vec::new(),
        }
    }

    /// Map a textual tree node identifier to an integer identifier
    pub(crate) fn id(&mut self, identifier: &'static str) -> Id {
        self.ids.get_or_intern(identifier)
    }

    /// Allocate or reuse a leaf string
    pub(crate) fn new_leaf(&mut self) -> String {
        self.leaf_cache
            .pop()
            .map(|mut s| {
                s.clear();
                s
            })
            .unwrap_or_default()
    }

    /// Allocate or reuse a Tree::List child vector
    pub(crate) fn new_list(&mut self) -> Vec<Tree> {
        self.list_cache
            .pop()
            .map(|mut v| {
                v.clear();
                v
            })
            .unwrap_or_default()
    }

    // TODO: Add method to display a TaggedTree, with configuration that's
    //       gradually fleshed out to include pretty-printing & simplification.
    //       This should integrate the functionality of recycle below
    //
    //       Consider having that method just take an impl TreeDisplay as a
    //       parameter, making Tree an implementation detail.
    //
    /// Discard a Tree in a manner that allows its allocations to be reused
    pub fn recycle(&mut self, (_id, tree): TaggedTree) {
        self.recycle_impl(tree)
    }
    //
    /// Implementation of recycle(), operating below the Id layer
    fn recycle_impl(&mut self, tree: Tree) {
        match tree {
            Tree::Leaf(leaf) => self.leaf_cache.push(leaf),
            Tree::List {
                header: _,
                item_id: _,
                mut items,
                separator: _,
                trailer: _,
            } => {
                for subtree in items.drain(..) {
                    self.recycle_impl(subtree);
                }
                self.list_cache.push(items);
            }
        }
    }
}

/// Basic data structure used for rendering
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Tree {
    /// Display without inner structure (e.g. single identifier)
    ///
    /// If style settings allow, this can still be shrunk through ellipses.
    // TODO: Implement that, and then replace truncate_path
    Leaf(String),

    /// Homogeneous list of things (e.g. function parameters, template
    /// parameters, scopes in nested name-specifiers...)
    List {
        /// Header ASCII char at list start (e.g. '<' for template parameters)
        header: Option<NonZeroU8>,

        /// Common Id shared by all list items
        item_id: Id,

        /// List items
        items: Vec<Tree>,

        /// How those items should be separated
        separator: Separator,

        /// Trailer ASCII char at list end (e.g. '>' for template parameters)
        trailer: Option<NonZeroU8>,
    },
    //
    // TODO: Also account for heterogeneous lists of things, e.g. the various
    //       components of an UnqualifiedId. Call that a tuple ?
    //
    // TODO: Go over all subparsers of cpparser until I think I can implement
    //       all existing displays.
}

/// Like a Tree, but indicates the nature of the thing being displayed
///
/// Enables priorizing the display of certain things over others.
///
pub type TaggedTree = (Id, Tree);

/// Data used to identify a part of a Tree
pub type Id = MiniSpur;

/// What to put inbetween list items
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Separator {
    /// Just whitespace
    ///
    /// ' ' in compact display, linefeed + reindent in sparse display.
    ///
    Space,

    /// A comma (','), followed by Space as defined above
    Comma,

    /// A scope separator ("::")
    Scope,

    /// A path separator ('/' on Unix, '\' on Windows)
    ///
    /// Note that Unix path root must not be followed by a separator.
    ///
    Path,
}

/// Method for displaying a C++ entity with advanced functionality
pub trait TreeDisplay {
    /// Get a tree-like visual representation for advanced display
    //
    // For tags, use the entity name without the trailing "View" : it serves no
    // purpose other than making parsing slightly more expensive.
    //
    fn tree_display(&self, context: &mut Context) -> TaggedTree;
}

// TODO: Add tests
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
