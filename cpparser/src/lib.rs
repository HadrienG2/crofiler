//! C++ entity name parsing

#![deny(missing_docs)]

pub mod anonymous;
pub mod functions;
pub mod names;
pub mod operators;
pub mod templates;
pub mod types;
pub mod values;

use crate::types::specifiers::legacy::{self, LegacyName};
use asylum::{
    lasso::{Rodeo, RodeoResolver},
    path::{InternedPath, InternedPaths, PathInterner, PathKey},
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::cell::{RefCell, RefMut};

/// Result type returned by C++ syntax parsers
pub type IResult<'a, O> = nom::IResult<&'a str, O, Error<&'a str>>;

/// Error type used by C++ syntax parsers
pub type Error<I> = nom::error::Error<I>;

/// Parser for C++ entities
pub fn entity(s: &str) -> IResult<Option<types::TypeLike>> {
    use nom::combinator::eof;
    let type_like = types::type_like.map(Some);
    let unknown = EntityParser::parse_unknown_entity.value(None);
    type_like.or(unknown).terminated(eof).parse(s)
}

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
    /// Interned identifiers
    identifiers: RefCell<Rodeo>,

    /// Legacy name parser
    legacy_name_parser: Box<dyn Fn(&str) -> IResult<LegacyName>>,

    /// Interned file paths
    paths: RefCell<PathInterner>,
}
//
impl EntityParser {
    /// Set up the parser
    pub fn new() -> Self {
        Self {
            identifiers: Default::default(),
            paths: Default::default(),
            legacy_name_parser: Box::new(legacy::legacy_name_parser()),
        }
    }

    /// Get access the file path interner
    ///
    /// This is needed when you have more file paths to intern which do not
    /// appear in C++ entity names, but may be related to the file paths
    /// appearing in C++ entity names.
    ///
    pub fn path_interner(&self) -> RefMut<PathInterner> {
        self.paths.borrow_mut()
    }

    /// Number of unique paths that have been interned so far
    pub fn num_paths(&self) -> usize {
        self.paths.borrow().len()
    }

    /// Total number of interned components across all interned paths so far
    pub fn num_path_components(&self) -> usize {
        self.paths.borrow().num_components()
    }

    /// Done parsing entities, just keep access to them
    pub fn finish(self) -> Entities {
        Entities {
            identifiers: self.identifiers.into_inner().into_resolver(),
            paths: self.paths.into_inner().finalize(),
        }
    }
}
//
/// Set of previously parsed C++ entities
#[derive(Debug, PartialEq)]
pub struct Entities {
    /// Identifiers
    identifiers: RodeoResolver,

    /// Paths
    paths: InternedPaths,
}
//
impl Entities {
    /// Retrieve a previously interned path
    pub fn path(&self, key: PathKey) -> InternedPath {
        self.paths.get(key)
    }
}
//
// TODO: Implement IntoIterator over all parsed entities

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    pub fn force_parse<'source, Output>(
        mut parser: impl Parser<&'source str, Output, Error<&'source str>>,
        source: &'source str,
    ) -> Output {
        let (rest, output) = parser.parse(source).unwrap();
        assert_eq!(rest, "");
        output
    }

    #[test]
    fn entity() {
        // Something that looks like a type name
        assert_eq!(
            super::entity("type_name"),
            Ok(("", Some(force_parse(types::type_like, "type_name"))))
        );

        // The infamous unknown clang entity
        assert_eq!(super::entity("<unknown>"), Ok(("", None)));
    }
}
