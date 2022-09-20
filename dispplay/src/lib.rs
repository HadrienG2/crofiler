mod entities;

use assert_matches::assert_matches;
use lasso::{MiniSpur, Rodeo};
use std::{
    collections::HashMap,
    num::{NonZeroU8, NonZeroUsize},
};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

// Goal: Hierarchical display structure that can be used for pretty-printing and
//       simplification
//
// Process: Make it work, then make it fast

/// State to be kept around while rendering multiple entities
#[derive(Debug, Eq, PartialEq)]
pub struct Context {
    /// Interner for tree node identifiers
    ids: Rodeo<Id>,

    /// Display configuration for various node types
    config: HashMap<Id, NodeConfig>,

    /// Cache of previously allocated leaf strings
    leaf_cache: Vec<String>,

    /// Cache of vectors of children for Tree::List
    list_cache: Vec<Vec<Tree>>,

    /// Indentation buffer
    indentation: String,
}
//
impl Context {
    /// Set up a rendering context
    pub fn new() -> Self {
        Self::default()
    }

    /// Adjust the display configuration options for a node type
    pub fn node_config(&mut self, key: &str) -> &mut NodeConfig {
        let id = self.id(key);
        self.node_config_raw(id)
    }

    /// Same but with the interned entry name already resolved
    fn node_config_raw(&mut self, id: Id) -> &mut NodeConfig {
        self.config.entry(id).or_default()
    }

    /// Compute a node's configuration, accounting for parent configuration
    fn full_config(&mut self, id: Id, parent: NodeConfig) -> NodeConfig {
        parent.with_child(*self.node_config_raw(id))
    }

    /// Display as much as possible of something's name within a fixed amount of
    /// terminal rows and columns
    pub fn display(&mut self, what: &impl TreeDisplay, cols: usize) -> String {
        let (root_id, root_tree) = what.tree(self);
        let root_config = self.full_config(root_id, NodeConfig::default());
        let mut result = String::new();
        let mut width_cache = HashMap::new();
        self.display_impl(&root_tree, &mut result, root_config, cols, &mut width_cache);
        assert_eq!(self.indentation.len(), 0);
        self.recycle(root_tree);
        result
    }

    /// Map a textual tree node identifier to an integer identifier
    pub(crate) fn id(&mut self, identifier: &str) -> Id {
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

    /// Implementation of display() operating directly on Trees
    fn display_impl(
        &mut self,
        tree: &Tree,
        target: &mut String,
        config: NodeConfig,
        cols: usize,
        width_cache: &mut HashMap<*const Tree, usize>,
    ) {
        assert!(cols >= 1);
        let flat_width = flat_display_width(width_cache, tree);
        if flat_width <= cols {
            flat_display(tree, target);
        } else {
            // TODO: Expand the idea of process_flat_display so that this can fit in
            match tree {
                Tree::Leaf(s) => {
                    if flat_width <= cols {
                        target.push_str(&s);
                    } else if flat_width >= config.abbreviation_threshold() {
                        let mut buf = std::mem::take(target).into_bytes();
                        abbreviate(&mut buf, &s, cols).expect("Write to Vec always succeeds");
                        *target = String::from_utf8(buf).expect("Should produced correct UTF-8");
                    } else {
                        target.push('…');
                    }
                }

                Tree::List {
                    header_trailer,
                    item_id,
                    items,
                    separator,
                } => {
                    if let Some((header, _trailer)) = header_trailer {
                        target.push(decode_bracket(*header));
                    }

                    const INDENT_DEPTH: usize = 2;
                    if cols > INDENT_DEPTH {
                        target.push('\n');
                        {
                            self.indentation
                                .extend(std::iter::repeat(' ').take(INDENT_DEPTH));
                            let cols = cols - INDENT_DEPTH;
                            let config = self.full_config(*item_id, config);
                            // FIXME: Deduplicate this recuring code
                            let mut need_first_separator = true;
                            if *separator == Separator::Path && !path_is_unix_rooted(&items[..]) {
                                need_first_separator = false;
                            };
                            let num_items = items.len();
                            for (idx, item) in items.iter().enumerate() {
                                target.push_str(&self.indentation);
                                let indentation_len = self.indentation.len();
                                self.display_impl(item, target, config, cols, width_cache);
                                debug_assert_eq!(self.indentation.len(), indentation_len);

                                let need_separator =
                                    idx < num_items - 1 && ((idx > 0) || need_first_separator);
                                if need_separator && !separator.after_line_feed() {
                                    target.push_str(separator.display());
                                }
                                target.push('\n');
                                if need_separator && separator.after_line_feed() {
                                    target.push_str(separator.display());
                                }
                            }
                        }
                        self.indentation
                            .truncate(self.indentation.len() - INDENT_DEPTH);
                        target.push_str(&self.indentation);
                    } else {
                        target.push('…');
                    }

                    if let Some((_header, trailer)) = header_trailer {
                        target.push(decode_bracket(*trailer));
                    }
                }
            }
        }
    }

    /// Recycle a tree's allocations for future reuse
    fn recycle(&mut self, tree: Tree) {
        match tree {
            Tree::Leaf(s) => {
                self.leaf_cache.push(s);
            }
            Tree::List {
                header_trailer: _,
                item_id: _,
                mut items,
                separator: _,
            } => {
                for item in items.drain(..) {
                    self.recycle(item)
                }
                self.list_cache.push(items);
            }
        }
    }
}
//
impl Default for Context {
    fn default() -> Self {
        let mut result = Self {
            ids: Rodeo::new(),
            config: HashMap::new(),
            leaf_cache: Vec::new(),
            list_cache: Vec::new(),
            indentation: String::new(),
        };
        result
            .node_config("Path")
            .set_priorize_right_list_side(true);
        result
            .node_config("Path.Component")
            .set_abbreviation_threshold(NonZeroUsize::new(15));
        result
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
        /// ASCII chars at list start and end
        ///
        /// For template parameters, that would be Some((b'<', b'>'))
        header_trailer: Option<(NonZeroU8, NonZeroU8)>,

        /// Common Id shared by all list items
        item_id: Id,

        /// List items
        items: Vec<Tree>,

        /// How those items should be separated
        separator: Separator,
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
    /// A comma (',')
    Comma,

    /// A scope separator ("::")
    Scope,

    /// A path separator ('/' on Unix, '\' on Windows)
    ///
    /// Note that Unix path root must not be followed by a separator.
    ///
    Path,
}
//
impl Separator {
    /// What the separator looks like, excluding trailing space
    fn display(self) -> &'static str {
        match self {
            Separator::Comma => ",",
            Separator::Scope => "::",
            Separator::Path => "/",
        }
    }

    /// Whether the separator should be combined with a space/newline
    fn needs_space(self) -> bool {
        match self {
            Separator::Comma => true,
            Separator::Scope => false,
            Separator::Path => false,
        }
    }

    /// Whether the separator should go after line feeds when those are needed
    fn after_line_feed(self) -> bool {
        match self {
            Separator::Comma => false,
            Separator::Scope => true,
            Separator::Path => false,
        }
    }
}

/// Method for displaying a C++ entity with advanced functionality
pub trait TreeDisplay {
    /// Get a tree-like visual representation for advanced display
    fn raw_tree(&self, context: &mut Context) -> Tree;

    /// Get a tag to be used for annotating display trees of this type
    ///
    /// # Notes to implementors
    ///
    /// Use the type name without "View" or "Interned" qualifiers: in this
    /// context, these provide no useful information and just make things more
    /// verbose than they need to be.
    ///
    /// If some entities are best thought of as sub-components of other
    /// entities, you can use the "X.Y" notation to denote this relationship,
    /// as in "Path.Component".
    ///
    fn tag() -> &'static str;

    /// Get the interned id used to refer to tag() in the Context
    fn id(context: &mut Context) -> Id {
        context.id(Self::tag())
    }

    /// Get a tagged display tree
    fn tree(&self, context: &mut Context) -> TaggedTree {
        (Self::id(context), self.raw_tree(context))
    }
}

/// Configuration options that apply to the display of individual tree nodes
///
/// Configuration applied to one node of the tree applies to all of its child
/// nodes unless if is overriden by said children. So applying some
/// configuration to the top-level "Entity" type applies it everywhere.
///
#[derive(Clone, Copy, Default, Debug, Eq, PartialEq)]
pub struct NodeConfig {
    /// Length threshold above which leaf strings can be abbreviated
    ///
    /// When this is left unset, leaf strings cannot be abbreviated :
    /// either the full string, '…' or nothing will be displayed.
    ///
    abbreviation_threshold: Option<Option<NonZeroUsize>>,

    /// Priorize display of the right-hand side of a list when short on space.
    ///
    /// Normally, when short on space, the left-hand side of a list is
    /// priorized as that follows standard typographic conventions ("f(x, …)"
    /// reads more naturally than "f(…, y)"). By setting this flag, you ask the
    /// display engine to priorize the right-hand side of the list instead,
    /// which makes sense for path-like lists.
    ///
    priorize_right_list_side: Option<bool>,
}
//
impl NodeConfig {
    /// Get length threshold above which leaf strings can be abbreviated
    fn abbreviation_threshold(&self) -> usize {
        self.abbreviation_threshold
            .unwrap_or_default()
            .map(|nzu| usize::from(nzu))
            .unwrap_or(usize::MAX)
    }

    /// Set length threshold above which leaf strings can be abbreviated
    fn set_abbreviation_threshold(&mut self, what: Option<NonZeroUsize>) {
        self.abbreviation_threshold = Some(what)
    }

    /// Get truth that the right-hand side of a list should be priorized
    fn should_priorize_right_list_side(&self) -> bool {
        self.priorize_right_list_side.unwrap_or_default()
    }

    /// Decide if the right-hand side of a list should be priorized
    fn set_priorize_right_list_side(&mut self, what: bool) {
        self.priorize_right_list_side = Some(what)
    }

    /// Merge child config into this parent config to get the full child config
    fn with_child(self, child: Self) -> Self {
        Self {
            abbreviation_threshold: child.abbreviation_threshold.or(self.abbreviation_threshold),
            priorize_right_list_side: child
                .priorize_right_list_side
                .or(self.priorize_right_list_side),
        }
    }
}

/// Truncate a string so that it only eats up n columns, by eating up the middle
/// Assumes absence of line feeds in the input string.
pub fn abbreviate(
    mut output: impl std::io::Write,
    input: &str,
    max_cols: usize,
) -> std::io::Result<()> {
    // Handle trivial case
    if input.width() <= max_cols {
        return write!(output, "{input}");
    }
    debug_assert!(input.chars().all(|c| c != '\r' && c != '\n'));

    // Make sure the request makes sense, set up common infrastructure
    assert!(max_cols >= 1);
    let bytes = input.as_bytes();
    let mut last_good = "";

    // Split our column budget into a header and trailer
    let max_header_cols = (max_cols - 1) / 2;
    let mut header_cols = 0;

    // Find a terminal header with the right number of columns
    let mut header_bytes = header_cols;
    loop {
        let header_candidate = std::str::from_utf8(&bytes[..header_bytes.into()]);
        if let Ok(candidate) = header_candidate {
            if candidate.width() > max_header_cols.into() {
                break;
            } else {
                header_cols = candidate.width();
                last_good = candidate;
            }
        }
        header_bytes += 1;
    }

    // Start printing out the result accordingly
    write!(output, "{last_good}…")?;

    // Find a terminal trailer with the right amount of columns
    let max_trailer_cols = max_cols - 1 - header_cols;
    let mut trailer_start = bytes.len() - usize::from(max_trailer_cols);
    loop {
        let trailer_candidate = std::str::from_utf8(&bytes[trailer_start..]);
        if let Ok(candidate) = trailer_candidate {
            if candidate.width() > max_trailer_cols.into() {
                break;
            } else {
                last_good = candidate;
            }
        }
        trailer_start -= 1;
    }

    // Emit the result
    write!(output, "{last_good}")
}

/// Compute a "flat" single-line display of a Tree
fn flat_display(tree: &Tree, target: &mut String) {
    process_flat_display(&tree, &mut |elem: &dyn Printable| {
        elem.append_to(target);
    });
}

/// Determine how many terminal columns flat_display() would use
fn flat_display_width(width_cache: &mut HashMap<*const Tree, usize>, tree: &Tree) -> usize {
    let tree_ptr: *const Tree = tree;
    if let Some(width) = width_cache.get(&tree_ptr) {
        return *width;
    }
    let mut width = 0;
    process_flat_display(tree, &mut |elem: &dyn Printable| {
        width += elem.unicode_width()
    });
    width_cache.insert(tree_ptr, width);
    width
}

/// Process the flat display of a Tree in an element-wise fashion
fn process_flat_display(tree: &Tree, process_elem: &mut impl FnMut(&dyn Printable)) {
    match tree {
        Tree::Leaf(s) => {
            let s: &str = &s;
            process_elem(&s);
        }

        Tree::List {
            header_trailer,
            item_id: _,
            items,
            separator,
        } => {
            if let Some((header, _trailer)) = header_trailer {
                process_elem(&decode_bracket(*header));
            }

            let mut need_first_separator = true;
            if *separator == Separator::Path && !path_is_unix_rooted(&items[..]) {
                need_first_separator = false;
            };

            let num_items = items.len();
            for (idx, item) in items.iter().enumerate() {
                process_flat_display(&item, process_elem);
                if idx < num_items - 1 && ((idx > 0) || need_first_separator) {
                    process_elem(&separator.display());
                    if separator.needs_space() {
                        process_elem(&' ');
                    }
                }
            }

            if let Some((_header, trailer)) = header_trailer {
                process_elem(&decode_bracket(*trailer));
            }
        }
    }
}

/// Asserting that a Tree::List's payload is a path, check if that path starts
/// with a Unix-style root (which musn't be followed by a separator)
fn path_is_unix_rooted(items: &[Tree]) -> bool {
    assert_matches!(
        items.first(), Some(Tree::Leaf(root)) => {
            root == "/"
        },
        "A path should be a sequence of 1+ unstructured components"
    )
}

/// Decode header/trailer ascii chars
fn decode_bracket(raw: NonZeroU8) -> char {
    let byte = u8::from(raw);
    debug_assert!(byte.is_ascii());
    byte as char
}

/// What to do when a C++ name gets over our terminal columns budget
enum Mode {
    /// Drop elements from the display to fit in one line
    OneLine,

    /// Use an indented display to reduce width
    ///
    /// If that does not suffice, then abbreviate if allowed or cut away with a
    /// … if nothing else works.
    ///
    Indented,
}

/// Things that can be appended to a String
trait Printable {
    /// Unicode width (as defined by the unicode_width crate)
    fn unicode_width(&self) -> usize;

    /// Append to string
    fn append_to(&self, s: &mut String);
}
//
impl Printable for char {
    fn unicode_width(&self) -> usize {
        UnicodeWidthChar::width(*self).unwrap_or(0)
    }

    fn append_to(&self, s: &mut String) {
        s.push(*self);
    }
}
//
impl<'a> Printable for &'a str {
    fn unicode_width(&self) -> usize {
        UnicodeWidthStr::width(*self)
    }

    fn append_to(&self, s: &mut String) {
        s.push_str(self);
    }
}

// TODO: Add tests
#[cfg(test)]
mod tests {
    use super::*;

    // TODO: Deduplicate TreeDisplay tests
}
