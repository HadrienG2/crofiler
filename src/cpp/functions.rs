//! Function-related parsing

use crate::cpp::{
    atoms,
    types::{
        self,
        qualifiers::{self, ConstVolatile, Reference},
        TypeLike,
    },
    IResult,
};
use nom::Parser;

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
    let parameters = separated_list0(space0.and(char(',')).and(space0), types::type_like);
    delimited(char('('), parameters, space0.and(char(')')))
        .map(Vec::into_boxed_slice)
        .parse(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpp::tests::force_parse_type;
    use pretty_assertions::assert_eq;

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
    fn function_parameters() {
        assert_eq!(super::function_parameters("()"), Ok(("", vec![].into())));
        assert_eq!(
            super::function_parameters("(signed char*)"),
            Ok(("", vec![force_parse_type("signed char*")].into()))
        );
        assert_eq!(
            super::function_parameters("(charamel<lol>&, T)"),
            Ok((
                "",
                vec![force_parse_type("charamel<lol>&"), force_parse_type("T")].into()
            ))
        );
    }
}
