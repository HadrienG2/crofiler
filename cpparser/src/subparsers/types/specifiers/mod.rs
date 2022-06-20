//! All about type specifiers
//!
//! See <https://en.cppreference.com/w/cpp/language/declarations> for context.

pub mod legacy;

use self::legacy::LegacyName;
use super::qualifiers::ConstVolatile;
use crate::{
    display::{CustomDisplay, DisplayState, RecursionDepths},
    subparsers::names::scopes::{IdExpression, IdExpressionView},
    Entities, EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

impl EntityParser {
    /// Parser recognizing type specifiers, as defined by
    /// <https://en.cppreference.com/w/cpp/language/declarations>
    #[inline]
    pub fn parse_type_specifier<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TypeSpecifier> {
        use nom::{
            character::complete::{space0, space1, u32},
            combinator::opt,
            sequence::preceded,
        };
        use nom_supreme::tag::complete::tag;

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

        // ...or an indexed auto type as sometimes seen in libiberty's output
        let libiberty_auto = preceded(tag("auto:"), u32).map(SimpleType::LibibertyAuto);

        // ...and we'll try all of that
        let simple_type = legacy_primitive.or(libiberty_auto).or(id_expression);

        // The simple type can be surrounded by cv qualifiers on both sides

        (Self::parse_cv.terminated(space0))
            .and(simple_type)
            .map(|(cv, simple_type)| TypeSpecifier { cv, simple_type })
            .parse(s)
    }
}
//
impl Entities {
    /// Access a previously parsed type specifier
    pub fn type_specifier(&self, ts: TypeSpecifier) -> TypeSpecifierView {
        TypeSpecifierView::new(ts, self)
    }

    /// Access a previously parsed simple type specifier
    pub(crate) fn simple_type(&self, st: SimpleType) -> SimpleTypeView {
        SimpleTypeView::new(st, self)
    }
}

/// Type specifier
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TypeSpecifier {
    /// CV qualifiers applying to the simple type
    cv: ConstVolatile,

    /// Simple type
    simple_type: SimpleType,
}
//
impl<T: Into<SimpleType>> From<T> for TypeSpecifier {
    fn from(simple_type: T) -> Self {
        Self {
            cv: ConstVolatile::default(),
            simple_type: simple_type.into(),
        }
    }
}

/// A view of a type specifier
pub struct TypeSpecifierView<'entities> {
    /// Wrapped TypeSpecifier
    inner: TypeSpecifier,

    /// Underlying interned entity storage
    entities: &'entities Entities,
}
//
impl<'entities> TypeSpecifierView<'entities> {
    /// Build a type specifier view
    pub fn new(inner: TypeSpecifier, entities: &'entities Entities) -> Self {
        Self { inner, entities }
    }

    /// CV qualifiers applying to the simple type
    pub fn cv(&self) -> ConstVolatile {
        self.inner.cv
    }

    /// Simple type
    pub fn simple_type(&self) -> SimpleTypeView {
        self.entities.simple_type(self.inner.simple_type)
    }
}
//
impl<'entities> PartialEq for TypeSpecifierView<'entities> {
    fn eq(&self, other: &Self) -> bool {
        (self.entities as *const Entities == other.entities as *const Entities)
            && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for TypeSpecifierView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for TypeSpecifierView<'entities> {
    fn recursion_depths(&self) -> RecursionDepths {
        self.simple_type().recursion_depths()
    }

    fn display(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        let cv = self.cv();
        if cv != ConstVolatile::default() {
            write!(f, "{cv} ")?;
        }
        self.simple_type().display(f, state)
    }
}

/// Inner simple type specifiers that TypeSpecifier can wrap
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum SimpleType {
    /// Id-expressions
    IdExpression(IdExpression),

    /// C-style space-separated type names (e.g. "unsigned int")
    LegacyName(LegacyName),

    /// Libiberty-style auto types (e.g. "auto:1")
    LibibertyAuto(u32),
}
//
impl From<IdExpression> for SimpleType {
    fn from(i: IdExpression) -> Self {
        Self::IdExpression(i)
    }
}
//
impl From<LegacyName> for SimpleType {
    fn from(n: LegacyName) -> Self {
        Self::LegacyName(n)
    }
}

/// View of a simple type specifier
#[derive(PartialEq)]
pub enum SimpleTypeView<'entities> {
    /// Id-expressions
    IdExpression(IdExpressionView<'entities>),

    /// C-style space-separated type names (e.g. "unsigned int")
    LegacyName(LegacyName),

    /// Libiberty-style auto types (e.g. "auto:1")
    LibibertyAuto(u32),
}
//
impl<'entities> SimpleTypeView<'entities> {
    /// Set up a simple type specifier view
    pub(crate) fn new(inner: SimpleType, entities: &'entities Entities) -> Self {
        match inner {
            SimpleType::IdExpression(i) => Self::IdExpression(entities.id_expression(i)),
            SimpleType::LegacyName(l) => Self::LegacyName(l),
            SimpleType::LibibertyAuto(u) => Self::LibibertyAuto(u),
        }
    }
}
//
impl<'entities> Display for SimpleTypeView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for SimpleTypeView<'entities> {
    fn recursion_depths(&self) -> RecursionDepths {
        match self {
            Self::IdExpression(i) => i.recursion_depths(),
            Self::LegacyName(_) => RecursionDepths::NEVER,
            Self::LibibertyAuto(_) => RecursionDepths::NEVER,
        }
    }

    fn display(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        match self {
            Self::IdExpression(i) => i.display(f, state),
            Self::LegacyName(l) => write!(f, "{l}"),
            Self::LibibertyAuto(u) => write!(f, "auto:{u}"),
        }
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
            Ok(("", id_expression("whatever").into()))
        );

        // Libiberty auto type branch
        assert_eq!(
            parser.parse_type_specifier("auto:1"),
            Ok(("", SimpleType::LibibertyAuto(1).into()))
        );

        // Legacy primitive branch
        assert_eq!(
            parser.parse_type_specifier("unsigned int"),
            Ok(("", LegacyName::UnsignedInt.into()))
        );

        // And we can live with the occasional keyword
        assert_eq!(
            parser.parse_type_specifier("const class MyClass"),
            Ok((
                "",
                TypeSpecifier {
                    simple_type: id_expression("MyClass").into(),
                    cv: ConstVolatile::CONST,
                }
            ))
        );
    }
}
