//! Types and other entities that follow the type grammar

pub mod declarators;
pub mod qualifiers;
pub mod specifiers;

use self::{declarators::DeclaratorKey, specifiers::TypeSpecifier};
use crate::{functions::FunctionArgumentsKey, Entities, EntityParser, IResult};
use asylum::lasso::Spur;
use nom::Parser;
use nom_supreme::ParserExt;

/// Interned C++ type key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// types as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a type by passing this key to the
/// type_like() method of the Entities struct.
///
pub type TypeKey = Spur;
//
impl EntityParser {
    /// Parser recognizing types (and some values that are indistinguishable from
    /// types without extra context).
    pub fn parse_type_like<'source>(&self, s: &'source str) -> IResult<'source, TypeKey> {
        use nom::{
            character::complete::{char, space0},
            combinator::opt,
            sequence::{delimited, tuple},
        };
        use nom_supreme::tag::complete::tag;

        // GNU-style type attributes come first
        let attributes = opt(delimited(
            tag("__attribute__("),
            |s| self.parse_function_call(s),
            char(')'),
        ))
        .map(|opt| opt.unwrap_or_else(|| self.function_arguments.entry().intern()));

        // Then come the type specifier and declarator
        tuple((
            attributes.terminated(space0),
            (|s| self.parse_type_specifier(s)).terminated(space0),
            |s| self.parse_declarator(s),
        ))
        .map(|(attributes, type_specifier, declarator)| {
            self.types.borrow_mut().intern(TypeLike {
                attributes,
                type_specifier,
                declarator,
            })
        })
        .parse(s)
    }

    /// Retrieve a type previously parsed by parse_type_like
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    #[cfg(test)]
    pub(crate) fn type_like(&self, key: TypeKey) -> TypeLike {
        self.types.borrow().get(key).clone()
    }

    /// Tell how many unique types have been parsed so far
    pub fn num_types(&self) -> usize {
        self.types.borrow().len()
    }
}
//
impl Entities {
    /// Retrieve a type previously parsed by parse_type_like
    pub fn type_like(&self, key: TypeKey) -> &TypeLike {
        self.types.get(key)
    }
}

/// A type name, or something looking close enough to it
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TypeLike {
    /// GNU-style attributes __attribute__((...))
    attributes: FunctionArgumentsKey,

    /// Type specifier
    type_specifier: TypeSpecifier,

    /// Declarator
    declarator: DeclaratorKey,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn type_like() {
        let parser = EntityParser::new();
        let parse_type_like = |s| {
            parser
                .parse_type_like(s)
                .map(|(rest, key)| (rest, parser.type_like(key)))
        };
        let attributes = |s| unwrap_parse(parser.parse_function_call(s));
        let type_specifier = |s| unwrap_parse(parser.parse_type_specifier(s));
        let declarator = |s| unwrap_parse(parser.parse_declarator(s));

        // Basic type specifier
        assert_eq!(
            parse_type_like("signed char"),
            Ok((
                "",
                TypeLike {
                    attributes: attributes("()"),
                    type_specifier: type_specifier("signed char"),
                    declarator: declarator("")
                }
            ))
        );

        // GNU-style attributes before
        assert_eq!(
            parse_type_like("__attribute__((unused)) long long"),
            Ok((
                "",
                TypeLike {
                    attributes: attributes("(unused)"),
                    type_specifier: type_specifier("long long"),
                    declarator: declarator("")
                }
            ))
        );

        // Basic function pointer
        assert_eq!(
            parse_type_like("something()"),
            Ok((
                "",
                TypeLike {
                    attributes: attributes("()"),
                    type_specifier: type_specifier("something"),
                    declarator: declarator("()"),
                }
            ))
        );

        // Fun template/expression ambiguity found during testing
        assert_eq!(
            parse_type_like("T<1>(U)"),
            Ok((
                "",
                TypeLike {
                    attributes: attributes("()"),
                    type_specifier: type_specifier("T<1>"),
                    declarator: declarator("(U)"),
                }
            ))
        );
    }
}
