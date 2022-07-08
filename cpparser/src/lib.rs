//! C++ entity name parsing

#![deny(missing_docs)]

pub mod display;
mod interning;
pub mod subparsers;

use crate::{
    display::{CustomDisplay, DisplayState},
    interning::recursion::RecursiveSequenceInterner,
    subparsers::{
        functions::{
            FunctionArgumentsKeyImpl, FunctionParametersKeyImpl, FUNCTION_ARGUMENTS_LEN_BITS,
            FUNCTION_PARAMETERS_LEN_BITS,
        },
        names::{
            atoms::IdentifierKey,
            scopes::{Scope, ScopesKeyImpl, SCOPES_LEN_BITS},
        },
        templates::{
            TemplateParameter, TemplateParameterListKeyImpl, TEMPLATE_PARAMETER_LIST_LEN_BITS,
        },
        types::{
            declarators::{DeclOperator, DeclaratorKeyImpl, DECLARATOR_LEN_BITS},
            specifiers::legacy::{self, LegacyName},
            TypeKey, TypeLike, TypeView,
        },
        values::{AfterValue, ValueKey, ValueLike, ValueTrailerKeyImpl, VALUE_TRAILER_LEN_BITS},
    },
};
use asylum::{
    lasso::{MiniSpur, Rodeo, RodeoResolver, Spur},
    path::{self, InternedPaths, PathInterner},
    sequence::InternedSequences,
    Interned, Interner,
};
use nom::{error::Error, Parser};
use nom_supreme::ParserExt;
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

/// Interned file path
pub type InternedPath<'entities> = path::InternedPath<'entities, PathComponentKey>;

/// Parser for C++ entities
//
// The toplevel module only contains the data structures and general
// functionality, parser-specific impl blocks can be found in individual modules
//
// Individual modules will define...
// - Appropriate key sizes and length types
// - Interning methods
// - Retrieval methods and unique entry count (for Entities)
//
pub struct EntityParser {
    /// Legacy name parser
    legacy_name_parser: Box<dyn Fn(&str) -> IResult<LegacyName>>,

    /// Interned identifiers
    identifiers: RefCell<Rodeo<IdentifierKey>>,

    /// Interned file paths
    paths: RefCell<PathInterner<PathComponentKey, PathKeyImpl, PATH_LEN_BITS>>,

    /// Interned types
    types: RefCell<Interner<TypeLike, TypeKey>>,

    /// Interned values
    values: RefCell<Interner<ValueLike, ValueKey>>,

    /// Interned template parameter lists
    template_parameter_lists: RecursiveSequenceInterner<
        TemplateParameter,
        TemplateParameterListKeyImpl,
        TEMPLATE_PARAMETER_LIST_LEN_BITS,
    >,

    /// Interned value trailers (part of ValueLike that comes after ValueHeader)
    value_trailers:
        RecursiveSequenceInterner<AfterValue, ValueTrailerKeyImpl, VALUE_TRAILER_LEN_BITS>,

    /// Interned function call arguments (sequences of values)
    function_arguments:
        RecursiveSequenceInterner<ValueKey, FunctionArgumentsKeyImpl, FUNCTION_ARGUMENTS_LEN_BITS>,

    /// Interned function parameter sets (sequences of types)
    function_parameters:
        RecursiveSequenceInterner<TypeKey, FunctionParametersKeyImpl, FUNCTION_PARAMETERS_LEN_BITS>,

    /// Interned sequences of scopes
    scope_sequences: RecursiveSequenceInterner<Scope, ScopesKeyImpl, SCOPES_LEN_BITS>,

    /// Interned declarators
    declarators: RecursiveSequenceInterner<DeclOperator, DeclaratorKeyImpl, DECLARATOR_LEN_BITS>,
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
    pub fn path_to_key(&mut self, path: &str) -> PathKey {
        self.path_to_key_imut(path)
    }

    /// Implementation of path_to_key with internal mutability
    pub(crate) fn path_to_key_imut(&self, path: &str) -> PathKey {
        self.paths
            .borrow_mut()
            .intern(path)
            .expect("Encountered relative (and thus non-interpretable) file path")
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
        let type_like = (|s| self.parse_type_like(s)).map(Some);
        let unknown = Self::parse_unknown_entity.value(None);
        final_parser(type_like.or(unknown).terminated(eof))(s)
    }

    /// Done parsing entities, just keep access to them
    pub fn finalize(self) -> Entities {
        Entities {
            identifiers: self.identifiers.into_inner().into_resolver(),
            paths: self.paths.into_inner().finalize(),
            types: self.types.into_inner().finalize(),
            values: self.values.into_inner().finalize(),
            template_parameter_lists: self.template_parameter_lists.into_inner().finalize(),
            value_trailers: self.value_trailers.into_inner().finalize(),
            function_arguments: self.function_arguments.into_inner().finalize(),
            function_parameters: self.function_parameters.into_inner().finalize(),
            scope_sequences: self.scope_sequences.into_inner().finalize(),
            declarators: self.declarators.into_inner().finalize(),
        }
    }
}
//
impl Default for EntityParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Set of previously parsed C++ entities
#[derive(Debug, PartialEq)]
pub struct Entities {
    /// Identifiers
    identifiers: RodeoResolver<IdentifierKey>,

    /// Paths
    paths: InternedPaths<PathComponentKey, PathKeyImpl, PATH_LEN_BITS>,

    /// Types
    types: Interned<TypeLike, TypeKey>,

    /// Values
    values: Interned<ValueLike, ValueKey>,

    /// Template parameter lists
    template_parameter_lists: InternedSequences<
        TemplateParameter,
        TemplateParameterListKeyImpl,
        TEMPLATE_PARAMETER_LIST_LEN_BITS,
    >,

    /// Value trailers (part of ValueLike that comes after ValueHeader)
    value_trailers: InternedSequences<AfterValue, ValueTrailerKeyImpl, VALUE_TRAILER_LEN_BITS>,

    /// Function call arguments (sequences of values)
    function_arguments:
        InternedSequences<ValueKey, FunctionArgumentsKeyImpl, FUNCTION_ARGUMENTS_LEN_BITS>,

    /// Function parameter sets (sequences of types)
    function_parameters:
        InternedSequences<TypeKey, FunctionParametersKeyImpl, FUNCTION_PARAMETERS_LEN_BITS>,

    /// Sequences of scopes
    scope_sequences: InternedSequences<Scope, ScopesKeyImpl, SCOPES_LEN_BITS>,

    /// Declarators (sequences of DeclOperator)
    declarators: InternedSequences<DeclOperator, DeclaratorKeyImpl, DECLARATOR_LEN_BITS>,
}
//
impl Entities {
    /// Retrieve a previously interned path
    pub fn path(&self, key: PathKey) -> InternedPath {
        self.paths.get(key)
    }

    /// Retrieve a previously interned entity
    pub fn entity(&self, key: EntityKey) -> EntityView {
        EntityView::new(key, self)
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
    pub fn new(inner: EntityKey, entities: &'entities Entities) -> Self {
        Self(inner.map(|ty| TypeView::new(ty, entities)))
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
        let parser = EntityParser::new();

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
