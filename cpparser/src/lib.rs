//! C++ entity name parsing

#![deny(missing_docs)]

pub mod anonymous;
pub mod functions;
pub mod names;
pub mod operators;
pub mod templates;
pub mod types;
mod utilities;
pub mod values;

use crate::{
    functions::{
        FunctionArgumentsKeyImpl, FunctionParametersKeyImpl, FUNCTION_ARGUMENTS_LEN_BITS,
        FUNCTION_PARAMETERS_LEN_BITS,
    },
    names::{
        atoms::IdentifierKey,
        scopes::{Scope, ScopesKeyImpl, SCOPES_LEN_BITS},
    },
    templates::{TemplateParameter, TemplateParametersKeyImpl, TEMPLATE_PARAMETERS_LEN_BITS},
    types::{
        declarators::{DeclOperator, DeclaratorKeyImpl, DECLARATOR_LEN_BITS},
        specifiers::legacy::{self, LegacyName},
        TypeKey, TypeLike,
    },
    utilities::RecursiveSequenceInterner,
    values::{AfterValue, ValueKey, ValueLike, ValueTrailerKeyImpl, VALUE_TRAILER_LEN_BITS},
};
use asylum::{
    lasso::{MiniSpur, Rodeo, RodeoResolver, Spur},
    path::{self, InternedPath, InternedPaths, PathInterner},
    sequence::InternedSequences,
    Interned, Interner,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::{cell::RefCell, fmt::Debug, path::Path};

/// Re-export asylum version in use
pub use asylum;

/// Result type returned by C++ syntax parsers
pub type IResult<'a, O> = nom::IResult<&'a str, O, Error<&'a str>>;

/// Error type used by C++ syntax parsers
pub type Error<I> = nom::error::Error<I>;

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

    /// Interned template parameter sets
    template_parameter_sets: RecursiveSequenceInterner<
        TemplateParameter,
        TemplateParametersKeyImpl,
        TEMPLATE_PARAMETERS_LEN_BITS,
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
    scopes: RecursiveSequenceInterner<Scope, ScopesKeyImpl, SCOPES_LEN_BITS>,

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
            template_parameter_sets: Default::default(),
            value_trailers: Default::default(),
            function_arguments: Default::default(),
            function_parameters: Default::default(),
            scopes: Default::default(),
            declarators: Default::default(),
        }
    }

    /// Intern a file path, returning the corresponding key
    ///
    /// This is exposed so that other file paths which are related to those
    /// appearing in C++ entity names, but appear in other context, can be
    /// interned using the same infrastructure for key comparability.
    ///
    pub fn path_to_key(&self, path: &str) -> PathKey {
        self.paths
            .borrow_mut()
            .intern(path)
            .expect("Encountered relative (and thus non-interpretable) file path")
    }

    /// Retrieve a previously interned path
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    pub(crate) fn path(&self, key: PathKey) -> Box<Path> {
        self.paths.borrow().get(key).to_boxed_path()
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
    pub fn parse_entity<'source>(&self, s: &'source str) -> IResult<'source, Option<TypeKey>> {
        use nom::combinator::eof;
        let type_like = (|s| self.parse_type_like(s)).map(Some);
        let unknown = Self::parse_unknown_entity.value(None);
        type_like.or(unknown).terminated(eof).parse(s)
    }

    /// Done parsing entities, just keep access to them
    pub fn finalize(self) -> Entities {
        Entities {
            identifiers: self.identifiers.into_inner().into_resolver(),
            paths: self.paths.into_inner().finalize(),
            types: self.types.into_inner().finalize(),
            values: self.values.into_inner().finalize(),
            template_parameter_sets: self.template_parameter_sets.into_inner().finalize(),
            value_trailers: self.value_trailers.into_inner().finalize(),
            function_arguments: self.function_arguments.into_inner().finalize(),
            function_parameters: self.function_parameters.into_inner().finalize(),
            scopes: self.scopes.into_inner().finalize(),
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
//
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

    /// Template parameter sets
    template_parameter_sets: InternedSequences<
        TemplateParameter,
        TemplateParametersKeyImpl,
        TEMPLATE_PARAMETERS_LEN_BITS,
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
    scopes: InternedSequences<Scope, ScopesKeyImpl, SCOPES_LEN_BITS>,

    /// Declarators (sequences of DeclOperator)
    declarators: InternedSequences<DeclOperator, DeclaratorKeyImpl, DECLARATOR_LEN_BITS>,
}
//
impl Entities {
    /// Retrieve a previously interned path
    pub fn path(&self, key: PathKey) -> InternedPath<PathComponentKey> {
        self.paths.get(key)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    pub fn unwrap_parse<Output>(res: IResult<Output>) -> Output {
        let (rest, output) = res.unwrap();
        assert_eq!(rest, "");
        output
    }

    #[test]
    fn entity() {
        let parser = EntityParser::new();

        // Something that looks like a type name
        assert_eq!(
            parser.parse_entity("type_name"),
            Ok(("", Some(unwrap_parse(parser.parse_type_like("type_name")))))
        );

        // The infamous unknown clang entity
        assert_eq!(parser.parse_entity("<unknown>"), Ok(("", None)));
    }
}
