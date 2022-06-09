//! Types and other entities that follow the type grammar

pub mod declarators;
pub mod qualifiers;
pub mod specifiers;

use self::{declarators::Declarator, specifiers::TypeSpecifier};
use crate::{values::ValueLike, EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parser recognizing types (and some values that are indistinguishable from
    /// types without extra context).
    pub fn parse_type_like<'source>(&self, s: &'source str) -> IResult<'source, TypeLike> {
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
        .map(|(attributes, type_specifier, declarator)| TypeLike {
            attributes,
            type_specifier,
            declarator,
        })
        .parse(s)
    }
}

/// A type name, or something looking close enough to it
// FIXME: This type appears in Box<T> and Box<[T]>, intern those once data is owned
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct TypeLike {
    /// GNU-style attributes __attribute__((...))
    attributes: Box<[ValueLike]>,

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
