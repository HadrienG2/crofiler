//! Types and other entities that follow the type grammar

use super::{
    atoms::{self, Reference},
    functions::{self, FunctionSignature},
    id_expression, IResult, IdExpression,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::ops::BitOr;

/// Parser recognizing types (and some values that are indistinguishable from
/// types without extra context), given a parser for the separator that is
/// expected to come after the type name.
pub fn type_like(
    s: &str,
    next_delimiter: impl FnMut(&str) -> IResult<()> + Copy,
) -> IResult<TypeLike> {
    use nom::combinator::peek;
    let id_expression = |s| type_like_impl(s, id_expression);
    fn legacy_id(s: &str) -> IResult<IdExpression> {
        legacy_primitive.map(IdExpression::from).parse(s)
    }
    let legacy_id = |s| type_like_impl(s, legacy_id);
    (id_expression.terminated(peek(next_delimiter)))
        .or(legacy_id.terminated(peek(next_delimiter)))
        .parse(s)
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
fn type_like_impl(s: &str, inner_id: impl Fn(&str) -> IResult<IdExpression>) -> IResult<TypeLike> {
    use nom::{
        character::complete::{char, space0, space1},
        combinator::opt,
        multi::many0,
        sequence::{preceded, tuple},
    };
    let bottom_cv = cv.terminated(space0);
    let pointer = preceded(space0.and(char('*')).and(space0), cv);
    let pointers = many0(pointer);
    let reference = preceded(space0, atoms::reference);
    let function_signature = preceded(space0, opt(functions::function_signature));
    let tuple = tuple((bottom_cv, inner_id, pointers, reference, function_signature));
    preceded(opt(atoms::keyword("typename").and(space1)), tuple)
        .map(
            |(bottom_cv, bottom_id, pointers, reference, function_signature)| TypeLike {
                bottom_cv,
                bottom_id,
                pointers: pointers.into_boxed_slice(),
                reference,
                function_signature,
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

    /// Layers of pointer indirection (* const * volatile...)
    pointers: Box<[ConstVolatile]>,

    /// Reference qualifiers
    reference: Reference,

    /// Function signature (for function pointers)
    function_signature: Option<FunctionSignature<'source>>,
}

/// Parser recognizing CV qualifiers
pub fn cv(s: &str) -> IResult<ConstVolatile> {
    use nom::{character::complete::space1, combinator::opt, sequence::preceded};
    let const_ = || {
        atoms::keyword("const").value(ConstVolatile {
            is_const: true,
            is_volatile: false,
        })
    };
    let volatile = || {
        atoms::keyword("volatile").value(ConstVolatile {
            is_const: false,
            is_volatile: true,
        })
    };
    opt((const_().and(opt(preceded(space1, volatile()))))
        .or(volatile().and(opt(preceded(space1, const_())))))
    .map(|opt_cv| {
        let (cv1, opt_cv2) = opt_cv.unwrap_or_default();
        let cv2 = opt_cv2.unwrap_or_default();
        cv1 | cv2
    })
    .parse(s)
}
//
/// CV qualifiers
#[derive(Default, Debug, PartialEq, Clone, Copy)]
pub struct ConstVolatile {
    /// Const qualifier
    is_const: bool,

    /// Volatile qualifier
    is_volatile: bool,
}
//
impl ConstVolatile {
    /// Lone const qualifier
    pub const CONST: ConstVolatile = ConstVolatile {
        is_const: true,
        is_volatile: false,
    };

    /// Lone volatile qualifier
    pub const VOLATILE: ConstVolatile = ConstVolatile {
        is_const: false,
        is_volatile: true,
    };
}
//
impl BitOr for ConstVolatile {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self {
            is_const: self.is_const | rhs.is_const,
            is_volatile: self.is_volatile | rhs.is_volatile,
        }
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

    fn whole_type(s: &str) -> IResult<TypeLike> {
        super::type_like(s, atoms::end_of_string)
    }

    #[test]
    fn type_like() {
        // Normal branch
        assert_eq!(
            whole_type("whatever"),
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
            whole_type("unsigned int"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("unsigned int"),
                    ..Default::default()
                }
            ))
        );

        // CV qualifiers
        assert_eq!(
            whole_type("const volatile unsigned"),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::CONST | ConstVolatile::VOLATILE,
                    bottom_id: IdExpression::from("unsigned"),
                    ..Default::default()
                }
            ))
        );

        // Basic pointer
        assert_eq!(
            whole_type("long int long*"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("long int long"),
                    pointers: vec![ConstVolatile::default()].into(),
                    ..Default::default()
                }
            ))
        );

        // Multiple pointers with CV qualifiers
        assert_eq!(
            whole_type("long double*const*"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("long double"),
                    pointers: vec![ConstVolatile::CONST, ConstVolatile::default()].into(),
                    ..Default::default()
                }
            ))
        );

        // Reference
        assert_eq!(
            whole_type("const anything&&"),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::CONST,
                    bottom_id: IdExpression::from("anything"),
                    reference: Reference::RValue,
                    ..Default::default()
                }
            ))
        );

        // Mixing references and pointers
        assert_eq!(
            whole_type("stuff*volatile*const&"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("stuff"),
                    pointers: vec![ConstVolatile::VOLATILE, ConstVolatile::CONST].into(),
                    reference: Reference::LValue,
                    ..Default::default()
                }
            ))
        );

        // Basic function pointer
        assert_eq!(
            whole_type("void()"),
            Ok((
                "",
                TypeLike {
                    bottom_id: IdExpression::from("void"),
                    function_signature: Some(FunctionSignature::default()),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn cv() {
        assert_eq!(super::cv(""), Ok(("", ConstVolatile::default())));
        assert_eq!(super::cv("const"), Ok(("", ConstVolatile::CONST)));
        assert_eq!(super::cv("volatile"), Ok(("", ConstVolatile::VOLATILE)));
        let const_volatile = ConstVolatile::CONST | ConstVolatile::VOLATILE;
        assert_eq!(super::cv("const volatile"), Ok(("", const_volatile)));
        assert_eq!(super::cv("volatile const"), Ok(("", const_volatile)));
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
