//! All about type specifiers
//!
//! See <https://en.cppreference.com/w/cpp/language/declarations> for context.

pub mod legacy;

use self::legacy::LegacyName;
use super::qualifiers::{self, ConstVolatile};
use crate::{
    names::scopes::{self, IdExpression},
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing type specifiers, as defined by
/// <https://en.cppreference.com/w/cpp/language/declarations>
pub fn type_specifier(s: &str) -> IResult<TypeSpecifier> {
    use nom::{
        character::complete::{space0, space1},
        combinator::opt,
        sequence::{preceded, tuple},
    };

    // The inner simple type can be an id-expression (which must be preceded
    // keywords in obscure circumstances...)
    let id_header =
        opt(
            EntityParser::keywords_parser(["typename", "class", "struct", "enum", "union"])
                .and(space1),
        );
    let id_expression = preceded(
        id_header,
        scopes::id_expression.map(SimpleType::IdExpression),
    );

    // ...or a legacy C-style primitive type with inner spaces...
    let legacy_primitive = legacy::legacy_name.map(SimpleType::LegacyName);

    // ...and we'll try all of that
    let simple_type = legacy_primitive.or(id_expression);

    // The simple type can be surrounded by cv qualifiers on both sides
    tuple((
        qualifiers::cv.terminated(space0),
        simple_type,
        preceded(space0, qualifiers::cv),
    ))
    .map(|(cv1, simple_type, cv2)| TypeSpecifier {
        cv: cv1 | cv2,
        simple_type,
    })
    .parse(s)
}
//
/// Type specifier
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct TypeSpecifier<'source> {
    /// CV qualifiers applying to the simple type
    cv: ConstVolatile,

    /// Simple type
    simple_type: SimpleType<'source>,
}
//
impl<'source, T: Into<SimpleType<'source>>> From<T> for TypeSpecifier<'source> {
    fn from(simple_type: T) -> Self {
        Self {
            cv: ConstVolatile::default(),
            simple_type: simple_type.into(),
        }
    }
}

/// Inner simple type specifiers that TypeSpecifier can wrap
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SimpleType<'source> {
    /// Id-expressions
    IdExpression(IdExpression<'source>),

    /// C-style space-separated type names (e.g. "unsigned int")
    LegacyName(LegacyName),
}
//
impl Default for SimpleType<'_> {
    fn default() -> Self {
        Self::IdExpression(IdExpression::default())
    }
}
//
impl<'source> From<IdExpression<'source>> for SimpleType<'source> {
    fn from(i: IdExpression<'source>) -> Self {
        Self::IdExpression(i)
    }
}
//
impl From<LegacyName> for SimpleType<'_> {
    fn from(n: LegacyName) -> Self {
        Self::LegacyName(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn type_specifier() {
        // Normal branch
        assert_eq!(
            super::type_specifier("whatever"),
            Ok((
                "",
                TypeSpecifier {
                    simple_type: SimpleType::IdExpression("whatever".into()),
                    ..Default::default()
                }
            ))
        );

        // Legacy primitive branch
        assert_eq!(
            super::type_specifier("unsigned int"),
            Ok((
                "",
                TypeSpecifier {
                    simple_type: SimpleType::LegacyName(LegacyName::UnsignedInt),
                    ..Default::default()
                }
            ))
        );

        // CV qualifiers are accepted before and after
        assert_eq!(
            super::type_specifier("const unsigned long volatile"),
            Ok((
                "",
                TypeSpecifier {
                    cv: ConstVolatile::CONST | ConstVolatile::VOLATILE,
                    simple_type: SimpleType::LegacyName(LegacyName::UnsignedLong),
                }
            ))
        );

        // And we can live with the occasional keyword
        assert_eq!(
            super::type_specifier("const class MyClass"),
            Ok((
                "",
                TypeSpecifier {
                    simple_type: SimpleType::IdExpression("MyClass".into()),
                    cv: ConstVolatile::CONST,
                }
            ))
        );
    }
}
