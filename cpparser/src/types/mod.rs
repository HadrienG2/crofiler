//! Types and other entities that follow the type grammar

pub mod declarators;
pub mod qualifiers;
pub mod specifiers;

use self::{declarators::Declarator, specifiers::TypeSpecifier};
use crate::{values::ValueKey, Entities, EntityParser, IResult};
use asylum::lasso::Spur;
use nom::Parser;
use nom_supreme::ParserExt;

/// Interned C++ type key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// types as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a type by passing it to the
/// type_like() method of the Entities struct.
///
// TODO: Adjust key size based on observed entry count
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
        .map(Option::unwrap_or_default);

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
// FIXME: This type appears in Box<[T]>, intern that once data is owned
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct TypeLike {
    /// GNU-style attributes __attribute__((...))
    attributes: Box<[ValueKey]>,

    /// Type specifier
    type_specifier: TypeSpecifier,

    /// Declarator
    declarator: Declarator,
}
//
impl<T: Into<TypeSpecifier>> From<T> for TypeLike {
    fn from(type_specifier: T) -> Self {
        Self {
            type_specifier: type_specifier.into(),
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{functions::FunctionSignature, tests::unwrap_parse};
    use pretty_assertions::assert_eq;

    #[test]
    fn type_like() {
        let parser = EntityParser::new();
        let type_specifier = |s| unwrap_parse(parser.parse_type_specifier(s));

        // Basic type specifier
        assert_eq!(
            parser.parse_type_like("signed char"),
            Ok(("", type_specifier("signed char").into()))
        );

        // GNU-style attributes before
        assert_eq!(
            parser.parse_type_like("__attribute__((unused)) long long"),
            Ok((
                "",
                TypeLike {
                    attributes: unwrap_parse(parser.parse_function_call("(unused)")),
                    type_specifier: type_specifier("long long"),
                    ..Default::default()
                }
            ))
        );

        // Basic function pointer
        assert_eq!(
            parser.parse_type_like("something()"),
            Ok((
                "",
                TypeLike {
                    type_specifier: type_specifier("something"),
                    declarator: vec![FunctionSignature::default().into()].into(),
                    ..Default::default()
                }
            ))
        );

        // Fun template/expression ambiguity found during testing
        assert_eq!(
            parser.parse_type_like("T<1>(U)"),
            Ok((
                "",
                TypeLike {
                    type_specifier: type_specifier("T<1>"),
                    declarator: vec![unwrap_parse(parser.parse_function_signature("(U)")).into()]
                        .into(),
                    ..Default::default()
                }
            ))
        );
    }
}
