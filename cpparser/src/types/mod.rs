//! Types and other entities that follow the type grammar

pub mod declarators;
pub mod qualifiers;
pub mod specifiers;

use self::{declarators::Declarator, specifiers::TypeSpecifier};
use crate::{functions, values::ValueLike, IResult};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing types (and some values that are indistinguishable from
/// types without extra context).
pub fn type_like(s: &str) -> IResult<TypeLike> {
    use nom::{
        character::complete::{char, space0},
        combinator::opt,
        sequence::{delimited, tuple},
    };
    use nom_supreme::tag::complete::tag;

    // GNU-style type attributes come first
    let attributes = opt(delimited(
        tag("__attribute__("),
        functions::function_call,
        char(')'),
    ))
    .map(Option::unwrap_or_default);

    // Then come the type specifier and declarator
    tuple((
        attributes.terminated(space0),
        specifiers::type_specifier.terminated(space0),
        declarators::declarator,
    ))
    .map(|(attributes, type_specifier, declarator)| TypeLike {
        attributes,
        type_specifier,
        declarator,
    })
    .parse(s)
}

/// A type name, or something looking close enough to it
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct TypeLike<'source> {
    /// GNU-style attributes __attribute__((...))
    attributes: Box<[ValueLike<'source>]>,

    /// Type specifier
    type_specifier: TypeSpecifier<'source>,

    /// Declarator
    declarator: Declarator<'source>,
}
//
impl<'source, T: Into<TypeSpecifier<'source>>> From<T> for TypeLike<'source> {
    fn from(type_specifier: T) -> Self {
        Self {
            type_specifier: type_specifier.into(),
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::declarators::DeclOperator;
    use super::*;
    use crate::{functions::FunctionSignature, tests::force_parse, values};
    use pretty_assertions::assert_eq;

    #[test]
    fn type_like() {
        // Basic type specifier
        assert_eq!(
            super::type_like("signed char"),
            Ok((
                "",
                TypeLike {
                    type_specifier: force_parse(specifiers::type_specifier, "signed char"),
                    ..Default::default()
                }
            ))
        );

        // GNU-style attributes before
        assert_eq!(
            super::type_like("__attribute__((unused)) long long"),
            Ok((
                "",
                TypeLike {
                    attributes: vec![force_parse(values::value_like::<false, false>, "unused")]
                        .into(),
                    type_specifier: force_parse(specifiers::type_specifier, "long long"),
                    ..Default::default()
                }
            ))
        );

        // Basic function pointer
        assert_eq!(
            super::type_like("something()"),
            Ok((
                "",
                TypeLike {
                    type_specifier: force_parse(specifiers::type_specifier, "something"),
                    declarator: vec![DeclOperator::Function(FunctionSignature::default())].into(),
                    ..Default::default()
                }
            ))
        );

        // Fun template/expression ambiguity found during testing
        assert_eq!(
            super::type_like("T<1>(U)"),
            Ok((
                "",
                TypeLike {
                    type_specifier: force_parse(specifiers::type_specifier, "T<1>"),
                    declarator: vec![DeclOperator::Function(force_parse(
                        functions::function_signature,
                        "(U)"
                    ))]
                    .into(),
                    ..Default::default()
                }
            ))
        );
    }
}