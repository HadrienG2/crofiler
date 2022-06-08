//! Function-related parsing

use crate::{
    names::atoms,
    types::{
        self,
        qualifiers::{ConstVolatile, Reference},
        TypeLike,
    },
    values::{self, ValueLike},
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::Debug;

impl EntityParser {
    /// Parser recognizing a function call
    pub fn parse_function_call(s: &str) -> IResult<Box<[ValueLike]>> {
        function_call(s)
    }

    /// Parser recognizing a function signature (parameters + qualifiers)
    pub fn parse_function_signature<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, FunctionSignature<'source, atoms::IdentifierKey, crate::PathKey>> {
        function_signature(s, &|s| self.parse_identifier(s), &|path| {
            self.path_to_key(path)
        })
    }
}

/// Parser recognizing a function call
// TODO: Make private once users are migrated
pub fn function_call(s: &str) -> IResult<Box<[ValueLike]>> {
    function_parameters(s, values::value_like::<false, true>)
}

/// Parser recognizing a function signature (parameters + qualifiers)
// TODO: Make private once users are migrated
pub fn function_signature<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, FunctionSignature<'source, IdentifierKey, PathKey>> {
    use nom::{
        character::complete::space0,
        combinator::opt,
        sequence::{preceded, tuple},
    };
    use nom_supreme::tag::complete::tag;

    let type_like = |s| types::type_like(s, parse_identifier, path_to_key);

    let function_parameters = |s| function_parameters(s, &type_like);

    let trailing_return = preceded(tag("->").and(space0), &type_like);

    let mut tuple = tuple((
        function_parameters.terminated(space0),
        EntityParser::parse_cv.terminated(space0),
        EntityParser::parse_reference.terminated(space0),
        opt(noexcept.terminated(space0)),
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
//
/// Function signature
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionSignature<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Parameter types
    parameters: Box<[TypeLike<'source, IdentifierKey, PathKey>]>,

    /// CV qualifiers
    cv: ConstVolatile,

    /// Reference qualifiers
    reference: Reference,

    /// noexcept qualifier
    ///
    /// The first layer of Option represents presence or absence of the
    /// "noexcept" keyword, the second layer represents the optional expression
    /// that can be passed as an argument to noexcept.
    noexcept: Option<Option<ValueLike<'source>>>,

    /// Trailing return type
    trailing_return: Option<TypeLike<'source, IdentifierKey, PathKey>>,
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > Default for FunctionSignature<'_, IdentifierKey, PathKey>
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

/// Parser recognizing the noexcept qualifier and its optional argument
fn noexcept(s: &str) -> IResult<Option<ValueLike>> {
    use nom::{
        character::complete::{char, space0},
        combinator::opt,
        sequence::{delimited, preceded},
    };
    preceded(
        EntityParser::keyword_parser("noexcept"),
        opt(delimited(
            char('(').and(space0),
            values::value_like::<false, true>,
            space0.and(char(')')),
        )),
    )
    .parse(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::force_parse;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    #[test]
    fn noexcept() {
        assert_eq!(super::noexcept("noexcept"), Ok(("", None)));
        assert_eq!(
            super::noexcept("noexcept(123)"),
            Ok((
                "",
                Some(force_parse(values::value_like::<false, false>, "123"))
            ))
        );
    }

    #[test]
    fn function_parameters() {
        let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);
        let type_parameters = |s| super::function_parameters(s, parse_type_like);
        assert_eq!(type_parameters("()"), Ok(("", vec![].into())));
        assert_eq!(
            type_parameters("(signed char*)"),
            Ok((
                "",
                vec![force_parse(parse_type_like, "signed char*")].into()
            ))
        );
        assert_eq!(
            type_parameters("(charamel<lol>&, T)"),
            Ok((
                "",
                vec![
                    force_parse(parse_type_like, "charamel<lol>&"),
                    force_parse(parse_type_like, "T")
                ]
                .into()
            ))
        );
    }

    #[test]
    fn function_signature() {
        let parse_function_signature =
            |s| super::function_signature(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_function_signature("()"),
            Ok(("", FunctionSignature::default()))
        );
        assert_eq!(
            parse_function_signature("() const"),
            Ok((
                "",
                FunctionSignature {
                    cv: ConstVolatile::CONST,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parse_function_signature("() &&"),
            Ok((
                "",
                FunctionSignature {
                    reference: Reference::RValue,
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parse_function_signature("() noexcept"),
            Ok((
                "",
                FunctionSignature {
                    noexcept: Some(None),
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parse_function_signature("() volatile &"),
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
            parse_function_signature("() volatile const noexcept"),
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
            parse_function_signature("() && noexcept(456)"),
            Ok((
                "",
                FunctionSignature {
                    reference: Reference::RValue,
                    noexcept: Some(Some(force_parse(values::value_like::<false, false>, "456"))),
                    ..Default::default()
                }
            ))
        );
        let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);
        assert_eq!(
            parse_function_signature("() -> int"),
            Ok((
                "",
                FunctionSignature {
                    trailing_return: Some(force_parse(parse_type_like, "int")),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn function_call() {
        assert_eq!(super::function_call("()"), Ok(("", vec![].into())));
        assert_eq!(
            super::function_call("(123)"),
            Ok(("", vec![123u8.into()].into()))
        );
        assert_eq!(
            super::function_call("(42, 'a')"),
            Ok(("", vec![42u8.into(), 'a'.into()].into()))
        );
    }
}
