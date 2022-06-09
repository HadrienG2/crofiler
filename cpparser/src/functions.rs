//! Function-related parsing

use crate::{
    names::atoms,
    types::{
        qualifiers::{ConstVolatile, Reference},
        TypeLike,
    },
    values::ValueLike,
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parser recognizing a function call
    pub fn parse_function_call<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, Box<[ValueLike<atoms::IdentifierKey, crate::PathKey>]>> {
        function_parameters(s, |s| self.parse_value_like(s, false, true))
    }

    /// Parser recognizing a function signature (parameters + qualifiers)
    pub fn parse_function_signature<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, FunctionSignature<atoms::IdentifierKey, crate::PathKey>> {
        use nom::{
            character::complete::space0,
            combinator::opt,
            sequence::{preceded, tuple},
        };
        use nom_supreme::tag::complete::tag;

        let type_like = |s| self.parse_type_like(s);

        let trailing_return = preceded(tag("->").and(space0), &type_like);

        let mut tuple = tuple((
            (|s| function_parameters(s, &type_like)).terminated(space0),
            Self::parse_cv.terminated(space0),
            Self::parse_reference.terminated(space0),
            opt((|s| self.parse_noexcept(s)).terminated(space0)),
            opt(trailing_return),
        ))
        .map(
            |(parameters, cv, reference, noexcept, trailing_return)| FunctionSignature {
                parameters,
                cv,
                reference,
                noexcept,
                trailing_return,
            },
        );

        tuple.parse(s)
    }

    /// Parser recognizing the noexcept qualifier and its optional argument
    fn parse_noexcept<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, Option<ValueLike<atoms::IdentifierKey, crate::PathKey>>> {
        use nom::{
            character::complete::{char, space0},
            combinator::opt,
            sequence::{delimited, preceded},
        };
        preceded(
            Self::keyword_parser("noexcept"),
            opt(delimited(
                char('(').and(space0),
                |s| self.parse_value_like(s, false, true),
                space0.and(char(')')),
            )),
        )
        .parse(s)
    }
}

/// Function signature
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionSignature<
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Parameter types
    parameters: Box<[TypeLike<IdentifierKey, PathKey>]>,

    /// CV qualifiers
    cv: ConstVolatile,

    /// Reference qualifiers
    reference: Reference,

    /// noexcept qualifier
    ///
    /// The first layer of Option represents presence or absence of the
    /// "noexcept" keyword, the second layer represents the optional expression
    /// that can be passed as an argument to noexcept.
    noexcept: Option<Option<ValueLike<IdentifierKey, PathKey>>>,

    /// Trailing return type
    trailing_return: Option<TypeLike<IdentifierKey, PathKey>>,
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > Default for FunctionSignature<IdentifierKey, PathKey>
{
    fn default() -> Self {
        Self {
            parameters: Default::default(),
            cv: Default::default(),
            reference: Default::default(),
            noexcept: Default::default(),
            trailing_return: Default::default(),
        }
    }
}

/// Parser recognizing a set of function parameters, given a parameter grammar
///
/// With a type grammar, this parses function signatures, and with a value
/// grammar, this parses function calls.
///
fn function_parameters<'source, T: 'source>(
    s: &'source str,
    parameter: impl FnMut(&'source str) -> IResult<'source, T>,
) -> IResult<Box<[T]>> {
    use nom::{
        character::complete::{char, space0},
        multi::separated_list0,
        sequence::delimited,
    };
    let parameters = separated_list0(space0.and(char(',')).and(space0), parameter);
    delimited(char('(').and(space0), parameters, space0.and(char(')')))
        .map(Vec::into_boxed_slice)
        .parse(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn noexcept() {
        let parser = EntityParser::new();
        assert_eq!(parser.parse_noexcept("noexcept"), Ok(("", None)));
        assert_eq!(
            parser.parse_noexcept("noexcept(123)"),
            Ok((
                "",
                Some(unwrap_parse(parser.parse_value_like("123", true, true)))
            ))
        );
    }

    #[test]
    fn function_parameters() {
        let parser = EntityParser::new();
        let parse_type_parameters =
            |s| super::function_parameters(s, &|s| parser.parse_type_like(s));
        let type_like = |s| unwrap_parse(parser.parse_type_like(s));
        assert_eq!(parse_type_parameters("()"), Ok(("", vec![].into())));
        assert_eq!(
            parse_type_parameters("(signed char*)"),
            Ok(("", vec![type_like("signed char*")].into()))
        );
        assert_eq!(
            parse_type_parameters("(charamel<lol>&, T)"),
            Ok(("", vec![type_like("charamel<lol>&"), type_like("T")].into()))
        );
    }

    #[test]
    fn function_signature() {
        let parser = EntityParser::new();
        assert_eq!(
            parser.parse_function_signature("()"),
            Ok(("", FunctionSignature::default()))
        );
        assert_eq!(
            parser.parse_function_signature("() const"),
            Ok((
                "",
                FunctionSignature {
                    cv: ConstVolatile::CONST,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() &&"),
            Ok((
                "",
                FunctionSignature {
                    reference: Reference::RValue,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() noexcept"),
            Ok((
                "",
                FunctionSignature {
                    noexcept: Some(None),
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() volatile &"),
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
            parser.parse_function_signature("() volatile const noexcept"),
            Ok((
                "",
                FunctionSignature {
                    cv: ConstVolatile::CONST | ConstVolatile::VOLATILE,
                    noexcept: Some(None),
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() && noexcept(456)"),
            Ok((
                "",
                FunctionSignature {
                    reference: Reference::RValue,
                    noexcept: Some(Some(unwrap_parse(
                        parser.parse_value_like("456", true, true),
                    ))),
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() -> int"),
            Ok((
                "",
                FunctionSignature {
                    trailing_return: Some(unwrap_parse(parser.parse_type_like("int"))),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn function_call() {
        let parser = EntityParser::new();
        assert_eq!(parser.parse_function_call("()"), Ok(("", vec![].into())));
        assert_eq!(
            parser.parse_function_call("(123)"),
            Ok(("", vec![123u8.into()].into()))
        );
        assert_eq!(
            parser.parse_function_call("(42, 'a')"),
            Ok(("", vec![42u8.into(), 'a'.into()].into()))
        );
    }
}
