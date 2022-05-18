//! Types and other entities that follow the type grammar

pub mod qualifiers;

use self::qualifiers::{ConstVolatile, PointersReference};
use crate::cpp::{
    atoms,
    functions::{self, FunctionSignature},
    names::{self, IdExpression},
    values::{self, ValueLike},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing types (and some values that are indistinguishable from
/// types without extra context)
pub fn type_like(s: &str) -> IResult<TypeLike> {
    let id_expression = |s| type_like_impl(s, names::id_expression);
    fn legacy_id(s: &str) -> IResult<IdExpression> {
        legacy_primitive.map(IdExpression::from).parse(s)
    }
    let legacy_id = |s| type_like_impl(s, legacy_id);
    (legacy_id.or(id_expression)).parse(s)
}

/// Parser recognizing types (and some values that are indistinguishable from
/// types without extra context), given an underlying type identifier parser
///
/// Concretely, this wraps either id_expression or legacy_primitive with extra
/// logic for CV qualifiers, pointers and references. Unfortunately, as a result
/// of the C++ grammar being the preposterous monster that it is, we cannot
/// fully decide at this layer of the parsing stack which of the id_expression
/// or legacy_primitive sub-parsers should be called.
///
/// Instead, we must reach the next delimiter character (e.g. ',' or '>' in
/// template parameter lists) before taking this decision. This is what the
/// higher-level type_like parser does.
fn type_like_impl(s: &str, bottom_id: impl Fn(&str) -> IResult<IdExpression>) -> IResult<TypeLike> {
    use nom::{
        character::complete::{char, space0, space1},
        combinator::{opt, verify},
        sequence::{delimited, preceded, tuple},
    };
    use qualifiers::pointers_reference;

    // C++ grammar requires that the bottom type name (along with its cv
    // qualifiers) be preceded with "typename" or "class" in some circumstances.
    let header = opt(atoms::keyword("typename")
        .or(atoms::keyword("class"))
        .and(space1));
    let bottom_type = delimited(
        header,
        tuple((
            qualifiers::cv.terminated(space0),
            bottom_id,
            preceded(space0, qualifiers::cv),
        ))
        .map(|(cv1, bottom_id, cv2)| (cv1 | cv2, bottom_id)),
        space0,
    );

    // Pointer and reference qualifiers must be surrounded with parentheses when
    // they are used to qualify function signatures or C-style arrays.
    //
    // This configuration must exclude the empty set of pointer and reference
    // qualifiers, otherwise, we'll accidentally match function pointers with
    // no argument like () as an empty set of pointers/references.
    //
    // We must also match the parenthesized version first, as otherwise the
    // unparenthesized pointer/reference matcher will successfully match nothing.
    let funcarr_pointers_reference = delimited(
        space0.and(char('(')).and(space0),
        verify(pointers_reference, |pr| pr != &PointersReference::default()),
        space0.and(char(')')),
    );
    let pointers_reference = funcarr_pointers_reference.or(pointers_reference);

    // Handle function pointers and C-style arrays
    let function_signature = preceded(space0, opt(functions::function_signature));
    let array = opt(delimited(
        space0.and(char('[')).and(space0),
        opt(values::value_like::<false>),
        space0.and(char(']')),
    ));

    // Put it all together
    let tuple = tuple((bottom_type, pointers_reference, function_signature, array));
    preceded(opt(atoms::keyword("typename").and(space1)), tuple)
        .map(
            |((bottom_cv, bottom_id), pointers_reference, function_signature, array)| TypeLike {
                bottom_cv,
                bottom_id,
                pointers_reference,
                function_signature,
                array,
            },
        )
        .parse(s)
}

/// A type name, or something looking close enough to it
#[derive(Debug, Default, PartialEq, Clone)]
pub struct TypeLike<'source> {
    /// CV qualifiers applying to the leftmost id-expression
    bottom_cv: ConstVolatile,

    /// Leftmost id-expression
    bottom_id: IdExpression<'source>,

    /// Pointer and reference qualifiers
    pointers_reference: PointersReference<'source>,

    /// Function signature (for function pointers)
    function_signature: Option<FunctionSignature<'source>>,

    /// C-style array
    ///
    /// There are two layers of option because there may or may not be array
    /// brackets, and within them, there may or may not be an array length.
    array: Option<Option<ValueLike<'source>>>,
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
pub fn legacy_primitive(s: &str) -> IResult<&str> {
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
    fn type_like() {
        // Normal branch
        assert_eq!(
            super::type_like("whatever"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("whatever"),
                    ..Default::default()
                }
            ))
        );

        // Legacy primitive branch
        assert_eq!(
            super::type_like("unsigned int"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("unsigned int"),
                    ..Default::default()
                }
            ))
        );

        // CV qualifiers before
        assert_eq!(
            super::type_like("const volatile unsigned"),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::CONST | ConstVolatile::VOLATILE,
                    bottom_id: IdExpression::from("unsigned"),
                    ..Default::default()
                }
            ))
        );

        // CV qualifiers after
        assert_eq!(
            super::type_like("char const"),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::CONST,
                    bottom_id: IdExpression::from("char"),
                    ..Default::default()
                }
            ))
        );

        // Pointer and reference qualifiers
        assert_eq!(
            super::type_like("stuff*volatile*const&"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("stuff"),
                    pointers_reference: qualifiers::pointers_reference("*volatile*const&")
                        .unwrap()
                        .1,
                    ..Default::default()
                }
            ))
        );

        // Basic function pointer
        assert_eq!(
            super::type_like("void()"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("void"),
                    function_signature: Some(FunctionSignature::default()),
                    ..Default::default()
                }
            ))
        );

        // Pointer to a function returning a void* pointer
        assert_eq!(
            super::type_like("void(*)()"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("void"),
                    pointers_reference: qualifiers::pointers_reference("*").unwrap().1,
                    function_signature: Some(FunctionSignature::default()),
                    ..Default::default()
                }
            ))
        );

        // Basic array
        assert_eq!(
            super::type_like("T[4]"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("T"),
                    array: Some(Some(4u8.into())),
                    ..Default::default()
                }
            ))
        );

        // Array of unknown length
        assert_eq!(
            super::type_like("T[]"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("T"),
                    array: Some(None),
                    ..Default::default()
                }
            ))
        );

        // Array of pointers and references
        assert_eq!(
            super::type_like("char const (*)[7]"),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::CONST,
                    bottom_id: IdExpression::from("char"),
                    pointers_reference: qualifiers::pointers_reference("*").unwrap().1,
                    array: Some(Some(7u8.into())),
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            super::type_like("char const (&)[7]"),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::CONST,
                    bottom_id: IdExpression::from("char"),
                    pointers_reference: qualifiers::pointers_reference("&").unwrap().1,
                    array: Some(Some(7u8.into())),
                    ..Default::default()
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
