//! All about type specifiers
//! (see https://en.cppreference.com/w/cpp/language/declarations for context)

use super::qualifiers::{self, ConstVolatile};
use crate::cpp::{
    atoms,
    names::{self, IdExpression},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing type specifiers, as defined by
/// https://en.cppreference.com/w/cpp/language/declarations
pub fn type_specifier(s: &str) -> IResult<TypeSpecifier> {
    use nom::{
        character::complete::{space0, space1},
        combinator::opt,
        sequence::{preceded, tuple},
    };

    // The C++ grammar requires that the simple type specifier (along with its cv
    // qualifiers) be preceded with keywords in some circumstances.
    let header = opt(atoms::keyword("typename")
        .or(atoms::keyword("class"))
        .or(atoms::keyword("struct"))
        .or(atoms::keyword("union"))
        .or(atoms::keyword("enum"))
        .and(space1));

    // The inner simple type can be an id-expression...
    let id_expression = names::id_expression.map(SimpleType::IdExpression);

    // ...or a legacy C-style primitive type with inner spaces...
    let legacy_primitive = legacy_primitive.map(SimpleType::LegacyPrimitive);

    // ...and we'll try all of that
    let simple_type = legacy_primitive.or(id_expression);

    // The simple type can be surrounded by cv qualifiers on both sides
    tuple((
        qualifiers::cv.terminated(space0),
        preceded(header, simple_type),
        preceded(space0, qualifiers::cv),
    ))
    .map(|(cv1, simple_type, cv2)| TypeSpecifier {
        cv: cv1 | cv2,
        simple_type,
    })
    .parse(s)
}

/// Type specifier
#[derive(Debug, Default, PartialEq, Clone)]
pub struct TypeSpecifier<'source> {
    /// CV qualifiers applying to the simple type
    cv: ConstVolatile,

    /// Simple type
    simple_type: SimpleType<'source>,
}

/// Inner simple type specifiers that TypeSpecifier can wrap
#[derive(Debug, PartialEq, Clone)]
pub enum SimpleType<'source> {
    /// Id-expressions
    IdExpression(IdExpression<'source>),

    /// C-style space-separated types (e.g. unsigned int)
    LegacyPrimitive(&'source str),
}
//
impl Default for SimpleType<'_> {
    fn default() -> Self {
        Self::IdExpression(IdExpression::default())
    }
}

/// Parser recognizing primitive types inherited from C, which can have spaces
/// in their name
///
/// This is not a full parser for C++ primitive types, as most of them can be
/// parsed with the regular identifier logic, and we do not need to single out
/// primitives in our processing.
///
/// It will also accept a bunch of types that are invalid from the point of view
/// of the C++ grammar, such as "long long short", for the sake of simplicity:
/// clang should not normally emit these, so we don't really care about
/// processing them right.
fn legacy_primitive(s: &str) -> IResult<&str> {
    use nom::{character::complete::space1, combinator::opt, multi::many0_count};
    use nom_supreme::tag::complete::tag;

    fn anything(s: &str) -> IResult<()> {
        let signedness = opt(tag("un")).and(atoms::keyword("signed")).value(());
        let size = atoms::keyword("short").or(atoms::keyword("long"));
        let base = atoms::keyword("int")
            .or(atoms::keyword("char"))
            .or(atoms::keyword("double"));
        signedness.or(size).or(base).parse(s)
    }

    // This is an allocation-free alternative to
    // separated_list1(space1, anything).recognize()
    (anything.and(many0_count(space1.and(anything))))
        .recognize()
        .parse(s)
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
                    simple_type: SimpleType::LegacyPrimitive("unsigned int"),
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
                    simple_type: SimpleType::LegacyPrimitive("unsigned long"),
                }
            ))
        );

        // And we can live with the occasional keyword
        assert_eq!(
            super::type_specifier("typename unsigned char"),
            Ok((
                "",
                TypeSpecifier {
                    simple_type: SimpleType::LegacyPrimitive("unsigned char"),
                    ..Default::default()
                }
            ))
        );

        // Keyword comes after cv qualifier
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

    #[test]
    fn legacy_primitive() {
        let test_legacy_primitive = |s| assert_eq!(super::legacy_primitive(s), Ok(("", s)));

        test_legacy_primitive("short int");
        test_legacy_primitive("unsigned short int");

        test_legacy_primitive("int");
        test_legacy_primitive("unsigned int");

        test_legacy_primitive("long int");
        test_legacy_primitive("unsigned long int");

        test_legacy_primitive("long long int");
        test_legacy_primitive("unsigned long long int");
        test_legacy_primitive("long int unsigned long");

        test_legacy_primitive("char");
        test_legacy_primitive("signed char");
        test_legacy_primitive("unsigned char");

        test_legacy_primitive("double");
        test_legacy_primitive("long double");
    }
}
