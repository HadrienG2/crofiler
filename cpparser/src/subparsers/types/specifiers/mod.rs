//! All about type specifiers
//!
//! See <https://en.cppreference.com/w/cpp/language/declarations> for context.

pub mod legacy;

use self::legacy::LegacyName;
use super::qualifiers::ConstVolatile;
use crate::{
    display::{CustomDisplay, DisplayState},
    subparsers::names::scopes::{IdExpression, IdExpressionView},
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};

impl EntityParser {
    /// Parser recognizing type specifiers, as defined by
    /// <https://en.cppreference.com/w/cpp/language/declarations>
    #[inline]
    pub fn parse_type_specifier<'source>(
        &mut self,
        s: &'source str,
    ) -> IResult<'source, TypeSpecifier> {
        self.parse_type_specifier_imut(s)
    }

    /// Implementation of parse_type_specifier using internal mutability
    #[inline]
    pub(crate) fn parse_type_specifier_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, TypeSpecifier> {
        use nom::character::complete::multispace0;
        (Self::parse_cv.terminated(multispace0))
            .and(|s| self.parse_simple_type_imut(s))
            .map(|(cv, simple_type)| TypeSpecifier { cv, simple_type })
            .parse(s)
    }

    /// Access a previously parsed type specifier
    pub fn type_specifier(&self, ts: TypeSpecifier) -> TypeSpecifierView {
        TypeSpecifierView::new(ts, self)
    }

    /// Parser recognizing simple types
    #[inline]
    fn parse_simple_type_imut<'source>(&self, s: &'source str) -> IResult<'source, SimpleType> {
        use nom::{
            character::complete::{multispace1, u32},
            combinator::opt,
            sequence::preceded,
        };
        use nom_supreme::tag::complete::tag;

        // The inner simple type can be an id-expression (which must be preceded
        // keywords in obscure circumstances...)
        let id_header =
            opt(
                Self::keywords_parser(["typename", "class", "struct", "enum", "union"])
                    .and(multispace1),
            );
        let id_expression = preceded(
            id_header,
            (|s| self.parse_id_expression_imut(s)).map(SimpleType::IdExpression),
        );

        // ...or a legacy C-style primitive type with inner spaces...
        let legacy_primitive = (|s| self.parse_legacy_name(s)).map(SimpleType::LegacyName);

        // ...or an indexed auto type as sometimes seen in libiberty's output
        let libiberty_auto = preceded(tag("auto:"), u32).map(SimpleType::LibibertyAuto);

        // ...and we'll try all of that
        legacy_primitive
            .or(libiberty_auto)
            .or(id_expression)
            .parse(s)
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
    entities: &'entities EntityParser,
}
//
impl<'entities> TypeSpecifierView<'entities> {
    /// Build a type specifier view
    pub fn new(inner: TypeSpecifier, entities: &'entities EntityParser) -> Self {
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
        (self.entities as *const _ == other.entities as *const _) && (self.inner == other.inner)
    }
}
//
impl<'entities> Display for TypeSpecifierView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for TypeSpecifierView<'entities> {
    fn recursion_depth(&self) -> usize {
        self.simple_type().recursion_depth()
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        let cv = self.cv();
        if cv != ConstVolatile::default() {
            write!(f, "{cv} ")?;
        }
        self.simple_type().display_impl(f, state)
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
    pub(crate) fn new(inner: SimpleType, entities: &'entities EntityParser) -> Self {
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
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl<'entities> CustomDisplay for SimpleTypeView<'entities> {
    fn recursion_depth(&self) -> usize {
        match self {
            Self::IdExpression(i) => i.recursion_depth(),
            Self::LegacyName(_) => 0,
            Self::LibibertyAuto(_) => 0,
        }
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        match self {
            Self::IdExpression(i) => i.display_impl(f, state),
            Self::LegacyName(l) => write!(f, "{l}"),
            Self::LibibertyAuto(u) => write!(f, "auto:{u}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{display::tests::check_custom_display, tests::unwrap_parse};
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_type() {
        let mut parser = EntityParser::new();
        let id_expression =
            |parser: &mut EntityParser, s| unwrap_parse(parser.parse_id_expression(s));
        let check_simple_type = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(parser.parse_simple_type_imut(input), Ok(("", expected)));
            check_custom_display(parser.simple_type(expected), displays);
        };

        // Normal branch
        let mut expected = id_expression(&mut parser, "whatever").into();
        check_simple_type(&mut parser, "whatever", expected, &["whatever"]);

        // Libiberty auto type branch
        check_simple_type(
            &mut parser,
            "auto:1",
            SimpleType::LibibertyAuto(1).into(),
            &["auto:1"],
        );

        // Legacy primitive branch
        check_simple_type(
            &mut parser,
            "unsigned int",
            LegacyName::UnsignedInt.into(),
            &["unsigned int"],
        );

        // And we can live with the occasional keyword
        expected = id_expression(&mut parser, "MyClass").into();
        check_simple_type(&mut parser, "class MyClass", expected, &["MyClass"]);
    }

    #[test]
    fn type_specifier() {
        let mut parser = EntityParser::new();
        let check_type_specifier = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(parser.parse_type_specifier(input), Ok(("", expected)));
            check_custom_display(parser.type_specifier(expected), displays);
        };

        // Works with a simple type specifier...
        check_type_specifier(
            &mut parser,
            "auto:42",
            SimpleType::LibibertyAuto(42).into(),
            &["auto:42"],
        );

        // ...as well as with extra CV qualifiers
        check_type_specifier(
            &mut parser,
            "const int",
            TypeSpecifier {
                simple_type: LegacyName::SignedInt.into(),
                cv: ConstVolatile::CONST,
            },
            &["const int"],
        );
    }
}
