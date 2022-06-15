//! Function-related parsing

use crate::{
    types::{
        qualifiers::{ConstVolatile, Reference},
        TypeKey,
    },
    utilities::RecursiveSequenceInterner,
    values::ValueKey,
    Entities, EntityParser, IResult,
};
use asylum::{
    lasso::{Key, MiniSpur, Spur},
    sequence::SequenceKey,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::{fmt::Debug, hash::Hash};

/// Interned function arguments (= list of parameter values) key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// argument lists as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve the parameter list by passing this key to
/// the function_arguments() method of the Entities struct.
///
pub type FunctionArgumentsKey = SequenceKey<FunctionArgumentsKeyImpl, FUNCTION_ARGUMENTS_LEN_BITS>;
pub(crate) type FunctionArgumentsKeyImpl = MiniSpur;
pub(crate) const FUNCTION_ARGUMENTS_LEN_BITS: u32 = 6;
//
/// Interned function parameter sets (= list of parameter types) key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// parameters lists as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve the parameter list by passing this key to
/// the function_parameters() method of the Entities struct.
///
pub type FunctionParametersKey =
    SequenceKey<FunctionParametersKeyImpl, FUNCTION_PARAMETERS_LEN_BITS>;
pub(crate) type FunctionParametersKeyImpl = Spur;
pub(crate) const FUNCTION_PARAMETERS_LEN_BITS: u32 = 8;
//
impl EntityParser {
    /// Parser recognizing a function call
    pub fn parse_function_call<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, FunctionArgumentsKey> {
        function_parameters(
            s,
            |s| self.parse_value_like(s, false, true),
            &self.function_arguments,
        )
    }

    /// Retrieve a function call previously parsed by parse_function_call
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    #[cfg(test)]
    pub(crate) fn function_arguments(&self, key: FunctionArgumentsKey) -> Box<[ValueKey]> {
        self.function_arguments.borrow().get(key).into()
    }

    /// Total number of function arguments across all interned function calls so far
    pub fn num_function_arguments(&self) -> usize {
        self.function_arguments.borrow().num_items()
    }

    /// Maximal number of function arguments in a single function call
    pub fn max_function_arguments_len(&self) -> Option<usize> {
        self.function_arguments.borrow().max_sequence_len()
    }

    /// Parser recognizing a function signature (parameters + qualifiers)
    pub fn parse_function_signature<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, FunctionSignature> {
        use nom::{
            character::complete::space0,
            combinator::opt,
            sequence::{preceded, tuple},
        };
        use nom_supreme::tag::complete::tag;

        let type_like = |s| self.parse_type_like(s);

        let trailing_return = preceded(tag("->").and(space0), &type_like);

        let mut tuple = tuple((
            (|s| function_parameters(s, &type_like, &self.function_parameters)).terminated(space0),
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

    /// Retrieve a function parameter set previously parsed by parse_function_signature
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    #[cfg(test)]
    pub(crate) fn function_parameters(&self, key: FunctionParametersKey) -> Box<[TypeKey]> {
        self.function_parameters.borrow().get(key).into()
    }

    /// Total number of function parameters across all interned function signatures so far
    pub fn num_function_parameters(&self) -> usize {
        self.function_parameters.borrow().num_items()
    }

    /// Maximal number of function parameters in a single function signature
    pub fn max_function_parameters_len(&self) -> Option<usize> {
        self.function_parameters.borrow().max_sequence_len()
    }

    /// Parser recognizing the noexcept qualifier and its optional argument
    fn parse_noexcept<'source>(&self, s: &'source str) -> IResult<'source, Option<ValueKey>> {
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
//
impl Entities {
    /// Retrieve a function call previously parsed by parse_function_call
    pub fn function_call(&self, key: FunctionArgumentsKey) -> &[ValueKey] {
        self.function_arguments.get(key)
    }

    /// Retrieve a function parameter set previously parsed by parse_function_signature
    pub fn function_parameters(&self, key: FunctionParametersKey) -> &[TypeKey] {
        self.function_parameters.get(key)
    }
}

/// Function signature
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct FunctionSignature {
    /// Parameter types
    parameters: FunctionParametersKey,

    /// CV qualifiers
    cv: ConstVolatile,

    /// Reference qualifiers
    reference: Reference,

    /// noexcept qualifier
    ///
    /// The first layer of Option represents presence or absence of the
    /// "noexcept" keyword, the second layer represents the optional expression
    /// that can be passed as an argument to noexcept.
    ///
    noexcept: Option<Option<ValueKey>>,

    /// Trailing return type
    trailing_return: Option<TypeKey>,
}
//
impl From<FunctionParametersKey> for FunctionSignature {
    fn from(parameters: FunctionParametersKey) -> Self {
        Self {
            parameters,
            cv: ConstVolatile::default(),
            reference: Reference::None,
            noexcept: None,
            trailing_return: None,
        }
    }
}

/// Parser recognizing a set of function parameters, given a parameter grammar
///
/// With a type grammar, this parses function signatures, and with a value
/// grammar, this parses function calls.
///
fn function_parameters<
    'source,
    T: Clone + Eq + Hash + 'source,
    KeyImpl: Key,
    const LEN_BITS: u32,
>(
    s: &'source str,
    parse_parameter: impl FnMut(&'source str) -> IResult<'source, T>,
    interner: &RecursiveSequenceInterner<T, KeyImpl, LEN_BITS>,
) -> IResult<'source, SequenceKey<KeyImpl, LEN_BITS>> {
    use nom::{
        character::complete::{char, space0},
        sequence::preceded,
    };
    use nom_supreme::multi::parse_separated_terminated;

    let arguments_header = char('(').and(space0);

    let non_empty_parameters = parse_separated_terminated(
        parse_parameter,
        space0.and(char(',')).and(space0),
        space0.and(char(')')),
        || interner.entry(),
        |mut entry, item| {
            entry.push(item);
            entry
        },
    )
    .map(|entry| entry.intern());

    let empty_parameters = char(')').map(|_| interner.entry().intern());

    preceded(arguments_header, non_empty_parameters.or(empty_parameters)).parse(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use assert_matches::assert_matches;
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
        let parse_type_parameters = |s| {
            super::function_parameters(
                s,
                &|s| parser.parse_type_like(s),
                &parser.function_parameters,
            )
        };
        let type_like = |s| unwrap_parse(parser.parse_type_like(s));
        let test_case = |parameters: &str, expected_types: &[&str]| {
            assert_matches!(parse_type_parameters(parameters), Ok(("", key)) => {
                let parameters = parser.function_parameters(key);
                assert_eq!(parameters.len(), expected_types.len());
                for (expected, actual) in expected_types.iter().zip(parameters.to_vec()) {
                    let expected = type_like(*expected);
                    assert_eq!(expected, actual);
                }
            })
        };
        test_case("()", &[]);
        test_case("(signed char*)", &["signed char*"]);
        test_case("(charamel<lol>&, T)", &["charamel<lol>&", "T"]);
    }

    #[test]
    fn function_signature() {
        let parser = EntityParser::new();
        let empty_parameters = parser.function_parameters.entry().intern();
        let type_parameters = |s| {
            unwrap_parse(super::function_parameters(
                s,
                &|s| parser.parse_type_like(s),
                &parser.function_parameters,
            ))
        };

        assert_eq!(
            parser.parse_function_signature("()"),
            Ok((
                "",
                FunctionSignature {
                    parameters: empty_parameters,
                    cv: ConstVolatile::default(),
                    reference: Reference::None,
                    noexcept: None,
                    trailing_return: None
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("(int)"),
            Ok((
                "",
                FunctionSignature {
                    parameters: type_parameters("int"),
                    cv: ConstVolatile::default(),
                    reference: Reference::None,
                    noexcept: None,
                    trailing_return: None
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() const"),
            Ok((
                "",
                FunctionSignature {
                    parameters: empty_parameters,
                    cv: ConstVolatile::CONST,
                    reference: Reference::None,
                    noexcept: None,
                    trailing_return: None
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() &&"),
            Ok((
                "",
                FunctionSignature {
                    parameters: empty_parameters,
                    cv: ConstVolatile::default(),
                    reference: Reference::RValue,
                    noexcept: None,
                    trailing_return: None
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() noexcept"),
            Ok((
                "",
                FunctionSignature {
                    parameters: empty_parameters,
                    cv: ConstVolatile::default(),
                    reference: Reference::None,
                    noexcept: Some(None),
                    trailing_return: None
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() volatile &"),
            Ok((
                "",
                FunctionSignature {
                    parameters: empty_parameters,
                    cv: ConstVolatile::VOLATILE,
                    reference: Reference::LValue,
                    noexcept: None,
                    trailing_return: None
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() volatile const noexcept"),
            Ok((
                "",
                FunctionSignature {
                    parameters: empty_parameters,
                    cv: ConstVolatile::CONST | ConstVolatile::VOLATILE,
                    reference: Reference::None,
                    noexcept: Some(None),
                    trailing_return: None
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() && noexcept(456)"),
            Ok((
                "",
                FunctionSignature {
                    parameters: empty_parameters,
                    cv: ConstVolatile::default(),
                    reference: Reference::RValue,
                    noexcept: Some(Some(unwrap_parse(
                        parser.parse_value_like("456", true, true),
                    ))),
                    trailing_return: None
                }
            ))
        );
        assert_eq!(
            parser.parse_function_signature("() -> int"),
            Ok((
                "",
                FunctionSignature {
                    parameters: empty_parameters,
                    cv: ConstVolatile::default(),
                    reference: Reference::None,
                    noexcept: None,
                    trailing_return: Some(unwrap_parse(parser.parse_type_like("int"))),
                }
            ))
        );
    }

    #[test]
    fn function_call() {
        let parser = EntityParser::new();
        let value_like = |s| unwrap_parse(parser.parse_value_like(s, true, true));
        let test_case = |arguments: &str, expected_values: &[&str]| {
            assert_matches!(parser.parse_function_call(arguments), Ok(("", key)) => {
                let arguments = parser.function_arguments(key);
                assert_eq!(arguments.len(), expected_values.len());
                for (expected, actual) in expected_values.iter().zip(arguments.to_vec()) {
                    let expected = value_like(*expected);
                    assert_eq!(expected, actual);
                }
            })
        };
        test_case("()", &[]);
        test_case("(123)", &["123"]);
        test_case("(42, 'a')", &["42", "'a'"]);
    }
}
