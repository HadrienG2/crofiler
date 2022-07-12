//! C++ entity name parsing

#![deny(missing_docs)]

pub mod display;
mod interning;
pub mod subparsers;

use crate::{
    display::{CustomDisplay, DisplayState},
    interning::recursion::RecursiveSequenceInterner,
    subparsers::{
        functions::{FunctionArgumentsKey, FunctionParametersKey},
        names::{
            atoms::IdentifierKey,
            scopes::{Scope, ScopesKey},
        },
        templates::{TemplateParameter, TemplateParameterListKey},
        types::{
            declarators::{DeclOperator, DeclaratorKey},
            specifiers::legacy::{self, LegacyName},
            TypeKey, TypeLike, TypeView,
        },
        values::{AfterValue, ValueKey, ValueLike, ValueTrailerKey},
    },
};
use asylum::{
    lasso::{MiniSpur, Rodeo, Spur},
    path, Interner,
};
use nom::{error::Error, Parser};
use nom_supreme::ParserExt;
use reffers::ARef;
use std::{
    cell::RefCell,
    fmt::{self, Display, Formatter},
};

/// Re-export asylum version in use
pub use asylum;

/// Result type returned by C++ syntax parsers
pub type IResult<'a, O> = nom::IResult<&'a str, O, Error<&'a str>>;

/// Re-export nom version in use
pub use nom;

/// Key to retrieve an interned path component
///
/// You can use this to compare path components more efficiently than via direct
/// string comparison, as it is guaranteed that if two keys differ, the
/// corresponding values differ as well.
///
pub type PathComponentKey = MiniSpur;

/// Interned file path key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// identifiers as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a path by passing this key to the
/// path() method of the Entities struct.
///
pub type PathKey = path::PathKey<PathKeyImpl, PATH_LEN_BITS>;
type PathKeyImpl = Spur;
const PATH_LEN_BITS: u32 = 8;
type PathInterner = path::PathInterner<PathComponentKey, PathKey>;

/// Interned file path
pub type InternedPath<'entities> = path::InternedPath<'entities, PathInterner>;

/// Parser for C++ entities
//
// --- ANYTHING BELOW THIS IS INTERNAL NOTES THAT WON'T END UP IN RUSTDOC ---
//
// # Code structure
//
// The toplevel module only contains the data structures and general
// functionality, parser-specific impl blocks can be found in individual modules
//
// Individual modules will define...
// - Appropriate key sizes and length types
// - Interning methods
// - Retrieval methods and unique entry count (for Entities)
//
// # Internal mutability
//
// Parser combinators do not mix well with stateful parsing, as the requirement
// not to capture &mut self in lambdas is too limiting. We cheat by using
// internal mutability, but in exchange, we pay the price of having runtime
// borrow checking failure modes, which have bad ergonomics.
//
// This price is acceptable in cpparser's implementation, but we don't want
// users to pay for it when they don't really need to.
//
// So we only use internal mutability internally (via pub(crate) parse_xyz_imut
// functions) and exposing a public API that honors Rust's regular mutability
// rules (via parse_xyz functions that require an &mut self to mutate state).
//
pub struct EntityParser {
    /// Legacy name parser
    legacy_name_parser: Box<dyn Send + Fn(&str) -> IResult<LegacyName>>,

    /// Interned identifiers
    identifiers: RefCell<Rodeo<IdentifierKey>>,

    /// Interned file paths
    paths: RefCell<PathInterner>,

    /// Interned types
    types: RefCell<Interner<TypeLike, TypeKey>>,

    /// Interned values
    values: RefCell<Interner<ValueLike, ValueKey>>,

    /// Interned template parameter lists
    template_parameter_lists:
        RecursiveSequenceInterner<TemplateParameter, TemplateParameterListKey>,

    /// Interned value trailers (part of ValueLike that comes after ValueHeader)
    value_trailers: RecursiveSequenceInterner<AfterValue, ValueTrailerKey>,

    /// Interned function call arguments (sequences of values)
    function_arguments: RecursiveSequenceInterner<ValueKey, FunctionArgumentsKey>,

    /// Interned function parameter sets (sequences of types)
    function_parameters: RecursiveSequenceInterner<TypeKey, FunctionParametersKey>,

    /// Interned sequences of scopes
    scope_sequences: RecursiveSequenceInterner<Scope, ScopesKey>,

    /// Interned declarators
    declarators: RecursiveSequenceInterner<DeclOperator, DeclaratorKey>,
}
//
impl EntityParser {
    /// Set up the parser
    pub fn new() -> Self {
        Self {
            legacy_name_parser: Box::new(legacy::legacy_name_parser()),
            identifiers: RefCell::new(Rodeo::new()),
            paths: Default::default(),
            types: Default::default(),
            values: RefCell::new(Interner::new()),
            template_parameter_lists: Default::default(),
            value_trailers: Default::default(),
            function_arguments: Default::default(),
            function_parameters: Default::default(),
            scope_sequences: Default::default(),
            declarators: Default::default(),
        }
    }

    /// Intern a file path, returning the corresponding key
    ///
    /// This is exposed so that other file paths which are related to those
    /// appearing in C++ entity names, but appear in other context, can be
    /// interned using the same infrastructure for key comparability.
    ///
    pub fn intern_path(&mut self, path: &str) -> PathKey {
        self.intern_path_imut(path)
    }

    /// Implementation of intern_path with internal mutability
    pub(crate) fn intern_path_imut(&self, path: &str) -> PathKey {
        self.paths
            .borrow_mut()
            .intern(path)
            .expect("Encountered relative (and thus non-interpretable) file path")
    }

    /// Retrieve a previously interned path
    pub fn path(&self, key: PathKey) -> InternedPath {
        InternedPath::new(ARef::new(self.paths.borrow()), key)
    }

    /// Total number of path components across all interned paths so far
    pub fn num_path_components(&self) -> usize {
        self.paths.borrow().num_components()
    }

    /// Total number of unique (interned) path components
    pub fn num_unique_path_components(&self) -> usize {
        self.paths.borrow().num_unique_components()
    }

    /// Maximal path length
    pub fn max_path_len(&self) -> Option<usize> {
        self.paths.borrow().max_path_len()
    }

    /// Parse a C++ entity
    ///
    /// None will be returned upon encountering the special `<unknown>` entity
    /// that clang occasionally feels like referring to.
    ///
    pub fn parse_entity<'source>(
        &mut self,
        s: &'source str,
    ) -> Result<EntityKey, nom::error::Error<&'source str>> {
        use nom::combinator::eof;
        use nom_supreme::final_parser::final_parser;
        let type_like = (|s| self.parse_type_like_imut(s)).map(Some);
        let unknown = Self::parse_unknown_entity.value(None);
        final_parser(type_like.or(unknown).terminated(eof))(s)
    }

    /// Retrieve a previously interned entity
    pub fn entity(&self, key: EntityKey) -> EntityView {
        EntityView::new(key, self)
    }
}
//
impl Default for EntityParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Interned C++ entity
///
/// None encodes to the special `<unknown>` entity that clang occasionally feels
/// like referring to.
///
pub type EntityKey = Option<TypeKey>;

/// View of a C++ entity
///
/// None encodes to the special `<unknown>` entity that clang occasionally feels
/// like referring to.
///
#[derive(PartialEq)]
pub struct EntityView<'entities>(pub Option<TypeView<'entities>>);
//
impl<'entities> EntityView<'entities> {
    /// Build a new-expression view
    pub fn new(inner: EntityKey, entities: &'entities EntityParser) -> Self {
        Self(inner.map(|ty| entities.type_like(ty)))
    }
}
//
impl<'entities> Display for EntityView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for EntityView<'entities> {
    fn recursion_depth(&self) -> usize {
        self.0.recursion_depth()
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        if let Some(ty) = &self.0 {
            ty.display_impl(f, state)
        } else {
            write!(f, "<unknown>")
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    pub fn unwrap_parse<Output>(res: IResult<Output>) -> Output {
        let (rest, output) = res.expect("This was a known-good parse which should not fail");
        assert_eq!(rest, "", "All output should have been consumed");
        output
    }

    #[test]
    fn entity() {
        let mut parser = EntityParser::new();

        // FIXME: Rework test harness to test CustomDisplay

        // Something that looks like a type name
        assert_eq!(
            parser.parse_entity("type_name"),
            Ok(Some(unwrap_parse(parser.parse_type_like("type_name"))))
        );

        // The infamous unknown clang entity
        assert_eq!(parser.parse_entity("<unknown>"), Ok(None));
    }
}
