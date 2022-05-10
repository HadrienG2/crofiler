//! Types and other entities that follow the type grammar

use super::{
    atoms::{self, ConstVolatile},
    id_expression, IdExpression,
};
use nom::IResult;

/// Parser recognizing types (and some values that are indistinguishable from
/// types without extra context), given a parser for the separator that is
/// expected to come after the type name.
pub fn type_like(
    s: &str,
    next_delimiter: impl FnMut(&str) -> IResult<&str, ()> + Copy,
) -> IResult<&str, TypeLike> {
    use nom::{
        branch::alt,
        combinator::{map, peek},
        sequence::terminated,
    };
    let id_expression = |s| type_like_impl(s, id_expression);
    fn legacy_id(s: &str) -> IResult<&str, IdExpression> {
        map(atoms::legacy_primitive, IdExpression::from)(s)
    }
    let legacy_id = |s| type_like_impl(s, legacy_id);
    alt((
        terminated(id_expression, peek(next_delimiter)),
        terminated(legacy_id, peek(next_delimiter)),
    ))(s)
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
fn type_like_impl(
    s: &str,
    inner_id: impl Fn(&str) -> IResult<&str, IdExpression>,
) -> IResult<&str, TypeLike> {
    use nom::{
        character::complete::{char, space0, space1},
        combinator::{map, opt},
        multi::{many0, many1_count},
        sequence::{pair, preceded, terminated, tuple},
    };
    let bottom_cv_opt = opt(terminated(atoms::cv, space1));
    let pointer_opt = preceded(pair(space0, char('*')), opt(preceded(space0, atoms::cv)));
    let pointer = map(pointer_opt, |cv| cv.unwrap_or_default());
    let pointers = many0(pointer);
    let num_refs_opt = opt(preceded(space0, many1_count(char('&'))));
    let tuple = tuple((bottom_cv_opt, inner_id, pointers, num_refs_opt));
    map(
        tuple,
        |(bottom_cv_opt, bottom_id, pointers, num_refs_opt)| TypeLike {
            bottom_cv: bottom_cv_opt.unwrap_or_default(),
            bottom_id,
            pointers: pointers.into_boxed_slice(),
            num_references: num_refs_opt.unwrap_or_default() as u8,
        },
    )(s)
}

/// A type name, or something looking close enough to it
#[derive(Debug, PartialEq, Clone)]
pub struct TypeLike<'source> {
    /// CV qualifiers applying to the leftmost id-expression
    bottom_cv: ConstVolatile,

    /// Leftmost id-expression
    bottom_id: IdExpression<'source>,

    /// Layers of pointer indirection (* const * volatile...)
    pointers: Box<[ConstVolatile]>,

    /// Number of final references between 0 (no reference) and 2 (rvalue)
    num_references: u8,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn end_of_string(s: &str) -> IResult<&str, ()> {
        use nom::combinator::{eof, map};
        map(eof, std::mem::drop)(s)
    }

    #[test]
    fn type_like() {
        // Normal branch
        assert_eq!(
            super::type_like("whatever", end_of_string),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::default(),
                    bottom_id: IdExpression::from("whatever"),
                    pointers: Default::default(),
                    num_references: 0
                }
            ))
        );

        // Legacy primitive branch
        assert_eq!(
            super::type_like("unsigned int", end_of_string),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::default(),
                    bottom_id: IdExpression::from("unsigned int"),
                    pointers: Default::default(),
                    num_references: 0
                }
            ))
        );

        // CV qualifiers
        assert_eq!(
            super::type_like("const volatile unsigned", end_of_string),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::CONST | ConstVolatile::VOLATILE,
                    bottom_id: IdExpression::from("unsigned"),
                    pointers: Default::default(),
                    num_references: 0
                }
            ))
        );

        // Basic pointer
        assert_eq!(
            super::type_like("long int long*", end_of_string),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::default(),
                    bottom_id: IdExpression::from("long int long"),
                    pointers: vec![ConstVolatile::default()].into(),
                    num_references: 0
                }
            ))
        );

        // Multiple pointers with CV qualifiers
        assert_eq!(
            super::type_like("long double*const*", end_of_string),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::default(),
                    bottom_id: IdExpression::from("long double"),
                    pointers: vec![ConstVolatile::CONST, ConstVolatile::default()].into(),
                    num_references: 0
                }
            ))
        );

        // Reference
        assert_eq!(
            super::type_like("const anything&&", end_of_string),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::CONST,
                    bottom_id: IdExpression::from("anything"),
                    pointers: Default::default(),
                    num_references: 2
                }
            ))
        );

        // Mixing references and pointers
        assert_eq!(
            super::type_like("stuff*volatile*const&", end_of_string),
            Ok((
                "",
                TypeLike {
                    bottom_cv: ConstVolatile::default(),
                    bottom_id: IdExpression::from("stuff"),
                    pointers: vec![ConstVolatile::VOLATILE, ConstVolatile::CONST].into(),
                    num_references: 1
                }
            ))
        );
    }
}
