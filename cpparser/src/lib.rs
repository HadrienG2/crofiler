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
use std::{cell::RefCell, fmt::Debug};

/// Result type returned by C++ syntax parsers
pub type IResult<'a, O> = nom::IResult<&'a str, O, Error<&'a str>>;

/// Error type used by C++ syntax parsers
pub type Error<I> = nom::error::Error<I>;

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

    /// Number of unique paths that have been interned so far
    pub fn num_paths(&self) -> usize {
        self.paths.borrow().len()
    }

    /// Total number of interned components across all interned paths so far
    pub fn num_path_components(&self) -> usize {
        self.paths.borrow().num_components()
    }

    /// Parse a C++ entity
    pub fn parse_entity<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, Option<types::TypeLike>> {
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
