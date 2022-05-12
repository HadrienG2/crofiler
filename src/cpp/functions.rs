//! Function-related parsing

use crate::cpp::{
    atoms::{self},
    types::{
        self,
        qualifiers::{self, ConstVolatile, Reference},
        TypeLike,
    },
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing a function signature (parameters + qualifiers)
pub fn function_signature(s: &str) -> IResult<FunctionSignature> {
    use nom::{
        character::complete::space0,
        combinator::opt,
        sequence::{preceded, tuple},
    };
    let cv = preceded(space0, qualifiers::cv);
    let reference = preceded(space0, qualifiers::reference);
    let noexcept_opt = preceded(space0, opt(atoms::keyword("noexcept")));
    tuple((function_parameters, cv, reference, noexcept_opt))
        .map(
            |(parameters, cv, reference, noexcept_opt)| FunctionSignature {
                parameters,
                cv,
                reference,
                noexcept: noexcept_opt.is_some(),
            },
        )
        .parse(s)
}
//
/// Function signature
#[derive(Clone, Default, Debug, PartialEq)]
pub struct FunctionSignature<'source> {
    /// Parameter types
    parameters: Box<[TypeLike<'source>]>,

    /// CV qualifiers
    cv: ConstVolatile,

    /// Reference qualifiers
    reference: Reference,

    /// noexcept qualifier
    noexcept: bool,
}

/// Parser recognizing a set of function parameters
fn function_parameters(s: &str) -> IResult<Box<[TypeLike]>> {
    use nom::{
        character::complete::{char, space0},
        multi::separated_list0,
        sequence::delimited,
    };
    let parameters = separated_list0(space0.and(char(',')).and(space0), function_parameter);
    delimited(char('('), parameters, space0.and(char(')')))
        .map(Vec::into_boxed_slice)
        .parse(s)
}

/// Parser recognizing a single function parameter
fn function_parameter(s: &str) -> IResult<TypeLike> {
    use nom::character::complete::{char, space0};
    fn delimiter(s: &str) -> IResult<()> {
        space0.and(char(',').or(char(')'))).value(()).parse(s)
    }
    types::type_like(s, delimiter)
}

#[cfg(test)]
mod tests {
    use super::super::tests::force_parse_type;
    use super::*;

    #[test]
    fn function_signature() {
        assert_eq!(
            super::function_signature("()"),
            Ok(("", FunctionSignature::default()))
        );
        assert_eq!(
            super::function_signature("() const"),
            Ok((
                "",
                FunctionSignature {
                    cv: ConstVolatile::CONST,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            super::function_signature("() &&"),
            Ok((
                "",
                FunctionSignature {
                    reference: Reference::RValue,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            super::function_signature("() noexcept"),
            Ok((
                "",
                FunctionSignature {
                    noexcept: true,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            super::function_signature("() volatile &"),
            Ok((
                "",
                FunctionSignature {
                    cv: ConstVolatile::VOLATILE,
                    reference: Reference::LValue,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            super::function_signature("() volatile const noexcept"),
            Ok((
                "",
                FunctionSignature {
                    cv: ConstVolatile::CONST | ConstVolatile::VOLATILE,
                    noexcept: true,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            super::function_signature("() && noexcept"),
            Ok((
                "",
                FunctionSignature {
                    reference: Reference::RValue,
                    noexcept: true,
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn function_parameter() {
        fn test_function_parameter_sep(text_wo_sep: &str, sep: &str, expected: TypeLike) {
            let mut text = text_wo_sep.to_owned();
            text.push_str(sep);
            assert_eq!(super::function_parameter(&text), Ok((sep, expected)));
        }
        fn test_function_parameter(text_wo_sep: &str) {
            let expected = force_parse_type(text_wo_sep);
            test_function_parameter_sep(text_wo_sep, ",", expected.clone());
            test_function_parameter_sep(text_wo_sep, ")", expected);
        }
        test_function_parameter("signed char*");
        test_function_parameter("charamel<lol>&");
    }

    #[test]
    fn function_parameters() {
        assert_eq!(super::function_parameters("()"), Ok(("", vec![].into())));
        assert_eq!(
            super::function_parameters("(A)"),
            Ok(("", vec![force_parse_type("A")].into()))
        );
        assert_eq!(
            super::function_parameters("(A, B)"),
            Ok((
                "",
                vec![force_parse_type("A"), force_parse_type("B")].into()
            ))
        );
    }
}
