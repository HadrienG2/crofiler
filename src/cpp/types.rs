//! Types and other entities that follow the type grammar

use super::{
    atoms::{self, ConstVolatile, Reference},
    functions::{self, FunctionSignature},
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
        multi::many0,
        sequence::{pair, preceded, terminated, tuple},
    };
    let bottom_cv_opt = opt(terminated(atoms::cv, space1));
    let pointer = preceded(pair(space0, char('*')), preceded(space0, atoms::cv));
    let pointers = many0(pointer);
    let reference = preceded(space0, atoms::reference);
    let function_signature = preceded(space0, opt(functions::function_signature));
    let tuple = tuple((
        bottom_cv_opt,
        inner_id,
        pointers,
        reference,
        function_signature,
    ));
    map(
        preceded(opt(pair(atoms::keyword("typename"), space1)), tuple),
        |(bottom_cv_opt, bottom_id, pointers, reference, function_signature)| TypeLike {
            bottom_cv: bottom_cv_opt.unwrap_or_default(),
            bottom_id,
            pointers: pointers.into_boxed_slice(),
            reference,
            function_signature,
        },
    )(s)
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

#[cfg(test)]
mod tests {
    use super::*;

    fn whole_type(s: &str) -> IResult<&str, TypeLike> {
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
}
