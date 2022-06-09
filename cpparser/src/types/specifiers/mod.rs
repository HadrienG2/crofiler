//! All about type specifiers
//!
//! See <https://en.cppreference.com/w/cpp/language/declarations> for context.

pub mod legacy;

use self::legacy::LegacyName;
use super::qualifiers::ConstVolatile;
use crate::{
    names::{atoms, scopes::IdExpression},
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parser recognizing type specifiers, as defined by
    /// <https://en.cppreference.com/w/cpp/language/declarations>
    pub fn parse_type_specifier<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TypeSpecifier<atoms::IdentifierKey, crate::PathKey>> {
        use nom::{
            character::complete::{space0, space1},
            combinator::opt,
            sequence::{preceded, tuple},
        };

        // The inner simple type can be an id-expression (which must be preceded
        // keywords in obscure circumstances...)
        let id_header =
            opt(
                Self::keywords_parser(["typename", "class", "struct", "enum", "union"]).and(space1),
            );
        let id_expression = preceded(
            id_header,
            (|s| self.parse_id_expression(s)).map(SimpleType::IdExpression),
        );

        // ...or a legacy C-style primitive type with inner spaces...
        let legacy_primitive = (|s| self.parse_legacy_name(s)).map(SimpleType::LegacyName);

        // ...and we'll try all of that
        let simple_type = legacy_primitive.or(id_expression);

        // The simple type can be surrounded by cv qualifiers on both sides
        tuple((
            Self::parse_cv.terminated(space0),
            simple_type,
            preceded(space0, Self::parse_cv),
        ))
        .map(|(cv1, simple_type, cv2)| TypeSpecifier {
            cv: cv1 | cv2,
            simple_type,
        })
        .parse(s)
    }
}

/// Type specifier
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeSpecifier<
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// CV qualifiers applying to the simple type
    cv: ConstVolatile,

    /// Simple type
    simple_type: SimpleType<IdentifierKey, PathKey>,
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
        T: Into<SimpleType<IdentifierKey, PathKey>>,
    > From<T> for TypeSpecifier<IdentifierKey, PathKey>
{
    fn from(simple_type: T) -> Self {
        Self {
            cv: ConstVolatile::default(),
            simple_type: simple_type.into(),
        }
    }
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > Default for TypeSpecifier<IdentifierKey, PathKey>
{
    fn default() -> Self {
        Self {
            cv: Default::default(),
            simple_type: Default::default(),
        }
    }
}

/// Inner simple type specifiers that TypeSpecifier can wrap
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SimpleType<
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Id-expressions
    IdExpression(IdExpression<IdentifierKey, PathKey>),

    /// C-style space-separated type names (e.g. "unsigned int")
    LegacyName(LegacyName),
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > Default for SimpleType<IdentifierKey, PathKey>
{
    fn default() -> Self {
        Self::IdExpression(IdExpression::default())
    }
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > From<IdExpression<IdentifierKey, PathKey>> for SimpleType<IdentifierKey, PathKey>
{
    fn from(i: IdExpression<IdentifierKey, PathKey>) -> Self {
        Self::IdExpression(i)
    }
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > From<LegacyName> for SimpleType<IdentifierKey, PathKey>
{
    fn from(n: LegacyName) -> Self {
        Self::LegacyName(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn type_specifier() {
        let parser = EntityParser::new();
        let id_expression = |s| unwrap_parse(parser.parse_id_expression(s));

        // Normal branch
        assert_eq!(
            parser.parse_type_specifier("whatever"),
            Ok((
                "",
                TypeSpecifier {
                    simple_type: SimpleType::IdExpression(id_expression("whatever")),
                    ..Default::default()
                }
            ))
        );

        // Legacy primitive branch
        assert_eq!(
            parser.parse_type_specifier("unsigned int"),
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
            parser.parse_type_specifier("const unsigned long volatile"),
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
            parser.parse_type_specifier("const class MyClass"),
            Ok((
                "",
                TypeSpecifier {
                    simple_type: SimpleType::IdExpression(id_expression("MyClass")),
                    cv: ConstVolatile::CONST,
                }
            ))
        );
    }
}
