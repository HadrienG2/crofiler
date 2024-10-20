//! Function-related parsing

use crate::{
    display::{CustomDisplay, DisplayState},
    interning::slice::SliceView,
    subparsers::{
        names::atoms::{IdentifierKey, IdentifierView},
        types::{
            qualifiers::{ConstVolatile, Reference},
            TypeKey, TypeView,
        },
        values::{ValueKey, ValueView},
    },
    EntityParser, IResult,
};
use asylum::{
    lasso::{MiniSpur, Spur},
    sequence::SequenceKey,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::{
    fmt::{self, Display, Formatter},
    hash::Hash,
};

#[cfg(test)]
use reffers::ARef;

/// Interned function arguments (= list of parameter values) key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// argument lists as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve the parameter list by passing this key to
/// the function_arguments() method of EntityParser.
///
pub type FunctionArgumentsKey = SequenceKey<FunctionArgumentsKeyImpl, FUNCTION_ARGUMENTS_LEN_BITS>;
type FunctionArgumentsKeyImpl = MiniSpur;
const FUNCTION_ARGUMENTS_LEN_BITS: u32 = 6;
//
/// Interned function parameter sets (= list of parameter types) key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// parameters lists as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve the parameter list by passing this key to
/// the function_parameters() method of EntityParser.
///
pub type FunctionParametersKey =
    SequenceKey<FunctionParametersKeyImpl, FUNCTION_PARAMETERS_LEN_BITS>;
type FunctionParametersKeyImpl = Spur;
const FUNCTION_PARAMETERS_LEN_BITS: u32 = 8;
//
impl EntityParser {
    /// Parser recognizing a function call
    pub fn parse_function_call<'source>(
        &mut self,
        s: &'source str,
    ) -> IResult<'source, FunctionArgumentsKey> {
        self.parse_function_call_imut(s)
    }

    /// Implementation of parse_function_call using internal mutability
    pub(crate) fn parse_function_call_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, FunctionArgumentsKey> {
        use nom::{
            character::complete::{char, multispace0},
            sequence::preceded,
        };
        use nom_supreme::multi::parse_separated_terminated;

        let arguments_header = char('(').and(multispace0);

        let non_empty_parameters = parse_separated_terminated(
            |s| self.parse_value_like_imut(s, false, true),
            multispace0.and(char(',')).and(multispace0),
            multispace0.and(char(')')),
            || self.function_arguments.entry(),
            |mut entry, item| {
                entry.push(item);
                entry
            },
        )
        .map(|entry| entry.intern());

        let empty_parameters = char(')').map(|_| self.function_arguments.entry().intern());

        preceded(arguments_header, non_empty_parameters.or(empty_parameters)).parse(s)
    }

    /// Access a previously parsed list of function arguments (from a function call)
    pub fn function_arguments(&self, a: FunctionArgumentsKey) -> FunctionArgumentsView {
        FunctionArgumentsView::new(a, self.function_arguments.borrow(), self)
    }

    /// Retrieve a function call previously parsed by parse_function_call
    #[cfg(test)]
    pub(crate) fn raw_function_arguments(&self, key: FunctionArgumentsKey) -> ARef<[ValueKey]> {
        self.function_arguments.get(key)
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
        &mut self,
        s: &'source str,
    ) -> IResult<'source, FunctionSignature> {
        self.parse_function_signature_imut(s)
    }

    /// Implementation of parse_function_signature using internal mutability
    pub(crate) fn parse_function_signature_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, FunctionSignature> {
        use nom::{
            character::complete::{char, multispace0},
            combinator::opt,
            sequence::{delimited, preceded, tuple},
        };
        use nom_supreme::tag::complete::tag;

        // ABI indicator (appears in demangled names)
        let abi = delimited(tag("[abi:"), |s| self.parse_identifier_imut(s), char(']'));

        let type_like = |s| self.parse_type_like_imut(s);
        let trailing_return = preceded(tag("->").and(multispace0), &type_like);

        let mut tuple = tuple((
            opt(abi),
            (|s| self.parse_function_parameter_set_imut(s)).terminated(multispace0),
            Self::parse_cv.terminated(multispace0),
            Self::parse_reference.terminated(multispace0),
            opt((|s| self.parse_noexcept_imut(s)).terminated(multispace0)),
            opt(trailing_return),
        ))
        .map(
            |(abi, parameter_set, cv, reference, noexcept, trailing_return)| FunctionSignature {
                abi,
                parameter_set,
                cv,
                reference,
                noexcept,
                trailing_return,
            },
        );

        tuple.parse(s)
    }

    /// Access a previously parsed function signature
    pub fn function_signature(&self, s: FunctionSignature) -> FunctionSignatureView {
        FunctionSignatureView::new(s, self)
    }

    /// Parser recognizing a function parameter set
    fn parse_function_parameter_set_imut<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, FunctionParameterSet> {
        use nom::{
            character::complete::{char, multispace0},
            sequence::preceded,
        };
        use nom_supreme::{multi::parse_separated_terminated, tag::complete::tag};

        let arguments_header = char('(').and(multispace0);

        let parameter_or_ellipsis = ((|s| self.parse_type_like_imut(s))
            .map(ParameterOrEllipsis::Parameter))
        .or(tag("...").value(ParameterOrEllipsis::Ellipsis));

        let non_empty_parameters = parse_separated_terminated(
            parameter_or_ellipsis,
            multispace0.and(char(',')).and(multispace0),
            multispace0.and(char(')')),
            || (self.function_parameters.entry(), false, false),
            |(mut entry, mut variadic, bad), item| {
                // A variadism ellipsis is only allowed in trailing position, so
                // there should be no call to this function after reaching it
                if variadic || bad {
                    (entry, variadic, true)
                } else {
                    match item {
                        ParameterOrEllipsis::Parameter(p) => entry.push(p),
                        ParameterOrEllipsis::Ellipsis => variadic = true,
                    };
                    (entry, variadic, false)
                }
            },
        )
        .verify(|(_entry, _ellipsis, bad)| !bad)
        .map(|(entry, ellipsis, _bad)| (entry.intern(), ellipsis));

        let empty_parameters =
            char(')').map(|_| (self.function_parameters.entry().intern(), false));

        preceded(arguments_header, non_empty_parameters.or(empty_parameters))
            .map(|(parameters, variadic)| FunctionParameterSet {
                parameters,
                variadic,
            })
            .parse(s)
    }

    /// Access a previously parsed list of function parameters
    pub(crate) fn function_parameters(&self, a: FunctionParametersKey) -> FunctionParametersView {
        FunctionParametersView::new(a, self.function_parameters.borrow(), self)
    }

    /// Retrieve a function parameter set previously parsed by parse_function_signature
    #[cfg(test)]
    pub(crate) fn raw_function_parameters(&self, key: FunctionParametersKey) -> ARef<[TypeKey]> {
        self.function_parameters.get(key)
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
    fn parse_noexcept_imut<'source>(&self, s: &'source str) -> IResult<'source, Option<ValueKey>> {
        use nom::{
            character::complete::{char, multispace0},
            combinator::opt,
            sequence::{delimited, preceded},
        };
        preceded(
            Self::keyword_parser("noexcept"),
            opt(delimited(
                char('(').and(multispace0),
                |s| self.parse_value_like_imut(s, false, true),
                multispace0.and(char(')')),
            )),
        )
        .parse(s)
    }
}

/// View of a function call (function argument set)
pub type FunctionArgumentsView<'entities> =
    SliceView<'entities, ValueKey, ValueView<'entities>, FunctionArgumentsKey>;

/// Function signature
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct FunctionSignature {
    /// ABI identifier (appears in demangled names)
    abi: Option<IdentifierKey>,

    /// Parameter types
    parameter_set: FunctionParameterSet,

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
impl From<FunctionParameterSet> for FunctionSignature {
    fn from(parameter_set: FunctionParameterSet) -> Self {
        Self {
            abi: None,
            parameter_set,
            cv: ConstVolatile::default(),
            reference: Reference::None,
            noexcept: None,
            trailing_return: None,
        }
    }
}

/// A view of a function signature
pub struct FunctionSignatureView<'entities> {
    /// Wrapped FunctionSignature
    inner: FunctionSignature,

    /// Underlying interned entity storage
    entities: &'entities EntityParser,
}
//
impl<'entities> FunctionSignatureView<'entities> {
    /// Build a new-expression view
    pub fn new(inner: FunctionSignature, entities: &'entities EntityParser) -> Self {
        Self { inner, entities }
    }

    /// ABI
    pub fn abi(&self) -> Option<IdentifierView> {
        self.inner.abi.map(|abi| self.entities.identifier(abi))
    }

    /// Parameter types
    pub fn parameters(&self) -> FunctionParametersView {
        self.entities
            .function_parameters(self.inner.parameter_set.parameters)
    }

    /// Is variadic (accepts extra parameters)
    pub fn variadic(&self) -> bool {
        self.inner.parameter_set.variadic
    }

    /// CV qualifiers
    pub fn cv(&self) -> ConstVolatile {
        self.inner.cv
    }

    /// Reference qualifiers
    pub fn reference(&self) -> Reference {
        self.inner.reference
    }

    /// noexcept qualifier
    ///
    /// The first layer of Option represents presence or absence of the
    /// "noexcept" keyword, the second layer represents the optional expression
    /// that can be passed as an argument to noexcept.
    ///
    pub fn noexcept(&self) -> Option<Option<ValueView>> {
        self.inner
            .noexcept
            .map(|o| o.map(|v| self.entities.value_like(v)))
    }

    /// Trailing return type
    pub fn trailing_return(&self) -> Option<TypeView> {
        self.inner
            .trailing_return
            .map(|t| self.entities.type_like(t))
    }
}
//
impl PartialEq for FunctionSignatureView<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.entities, other.entities) && (self.inner == other.inner)
    }
}
//
impl Display for FunctionSignatureView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display_impl(f, &DisplayState::default())
    }
}
//
impl CustomDisplay for FunctionSignatureView<'_> {
    fn recursion_depth(&self) -> usize {
        self.parameters()
            .recursion_depth()
            .max(self.noexcept().recursion_depth())
            .max(self.trailing_return().recursion_depth())
    }

    fn display_impl(&self, f: &mut Formatter<'_>, state: &DisplayState) -> Result<(), fmt::Error> {
        if let Some(abi) = self.abi() {
            write!(f, "[abi:{abi}]")?;
        }

        self.parameters().display_impl(f, state)?;
        if state.can_recurse() && self.variadic() {
            write!(f, ", ...")?;
        }
        write!(f, ")")?;

        let cv = self.cv();
        if cv != ConstVolatile::default() {
            write!(f, " {cv}")?;
        }

        let reference = self.reference();
        if reference != Reference::None {
            write!(f, " {reference}")?;
        }

        let noexcept = self.noexcept();
        if let Some(value) = noexcept {
            write!(f, " noexcept")?;
            if let Some(value) = value {
                write!(f, "(")?;
                value.display_impl(f, state)?;
                write!(f, ")")?;
            }
        }

        let trailing_return = self.trailing_return();
        if let Some(ty) = trailing_return {
            write!(f, " -> ")?;
            ty.display_impl(f, state)?;
        }
        Ok(())
    }
}

/// View of a function parameter set
pub type FunctionParametersView<'entities> =
    SliceView<'entities, TypeKey, TypeView<'entities>, FunctionParametersKey>;

/// Function parameter of ... ellipsis sign
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum ParameterOrEllipsis {
    Parameter(TypeKey),
    Ellipsis,
}
//
/// Function parameter set
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct FunctionParameterSet {
    /// Sequence of parameters
    parameters: FunctionParametersKey,

    /// Truth that the function is variadic (allows more parameters)
    variadic: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{display::tests::check_custom_display, tests::unwrap_parse};
    use assert_matches::assert_matches;
    use pretty_assertions::assert_eq;

    #[test]
    fn noexcept() {
        let mut parser = EntityParser::new();
        assert_eq!(parser.parse_noexcept_imut("noexcept"), Ok(("", None)));
        assert_eq!(
            parser.parse_noexcept_imut("noexcept(123)"),
            Ok((
                "",
                Some(unwrap_parse(parser.parse_value_like("123", true, true)))
            ))
        );
    }

    #[test]
    fn function_parameter_set() {
        let mut parser = EntityParser::new();
        let mut test_case = |parameters, expected_types: &[&str], expected_variadic| {
            assert_matches!(parser.parse_function_parameter_set_imut(
                parameters
            ), Ok(("", parameter_set)) => {
                let parameters = parser.raw_function_parameters(parameter_set.parameters).to_owned();
                assert_eq!(parameters.len(), expected_types.len());
                for (expected, actual) in expected_types.iter().zip(parameters.to_vec()) {
                    let expected = unwrap_parse(parser.parse_type_like(expected));
                    assert_eq!(expected, actual);
                }
                assert_eq!(parameter_set.variadic, expected_variadic);
            })
        };
        test_case("()", &[], false);
        test_case("(...)", &[], true);
        test_case("(signed char*)", &["signed char*"], false);
        test_case("(signed char*, ...)", &["signed char*"], true);
        test_case("(charamel<lol>&, T)", &["charamel<lol>&", "T"], false);
        test_case("(charamel<lol>&, T, ...)", &["charamel<lol>&", "T"], true);
    }

    #[test]
    fn function_signature() {
        let mut parser = EntityParser::new();
        let parameter_set = |parser: &mut EntityParser, input| {
            unwrap_parse(parser.parse_function_parameter_set_imut(input))
        };
        let check_function_signature = |parser: &mut EntityParser, input, expected, displays| {
            assert_eq!(parser.parse_function_signature(input), Ok(("", expected)));
            check_custom_display(parser.function_signature(expected), displays);
        };

        let mut expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::default(),
            reference: Reference::None,
            noexcept: None,
            trailing_return: None,
        };
        check_function_signature(&mut parser, "()", expected, &["()"]);

        expected = FunctionSignature {
            abi: Some(unwrap_parse(parser.parse_identifier("cxx11"))),
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::default(),
            reference: Reference::None,
            noexcept: None,
            trailing_return: None,
        };
        check_function_signature(&mut parser, "[abi:cxx11]()", expected, &["[abi:cxx11]()"]);

        expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "(int)"),
            cv: ConstVolatile::default(),
            reference: Reference::None,
            noexcept: None,
            trailing_return: None,
        };
        check_function_signature(&mut parser, "(int)", expected, &["(…)", "(int)"]);

        expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::CONST,
            reference: Reference::None,
            noexcept: None,
            trailing_return: None,
        };
        check_function_signature(&mut parser, "() const", expected, &["() const"]);

        expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::default(),
            reference: Reference::RValue,
            noexcept: None,
            trailing_return: None,
        };
        check_function_signature(&mut parser, "() &&", expected, &["() &&"]);

        expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::default(),
            reference: Reference::None,
            noexcept: Some(None),
            trailing_return: None,
        };
        check_function_signature(&mut parser, "() noexcept", expected, &["() noexcept"]);

        expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::VOLATILE,
            reference: Reference::LValue,
            noexcept: None,
            trailing_return: None,
        };
        check_function_signature(&mut parser, "() volatile &", expected, &["() volatile &"]);

        expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::CONST | ConstVolatile::VOLATILE,
            reference: Reference::None,
            noexcept: Some(None),
            trailing_return: None,
        };
        check_function_signature(
            &mut parser,
            "() volatile const noexcept",
            expected,
            &["() const volatile noexcept"],
        );

        expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::default(),
            reference: Reference::RValue,
            noexcept: Some(Some(unwrap_parse(
                parser.parse_value_like("456", true, true),
            ))),
            trailing_return: None,
        };
        check_function_signature(
            &mut parser,
            "() && noexcept(456)",
            expected,
            &["() && noexcept(456)"],
        );

        expected = FunctionSignature {
            abi: None,
            parameter_set: parameter_set(&mut parser, "()"),
            cv: ConstVolatile::default(),
            reference: Reference::None,
            noexcept: None,
            trailing_return: Some(unwrap_parse(parser.parse_type_like("int"))),
        };
        check_function_signature(&mut parser, "() -> int", expected, &["() -> int"]);
    }

    #[test]
    fn function_call() {
        let mut parser = EntityParser::new();

        let mut test_case = |arguments, expected_values: &[&str], displays| {
            assert_matches!(parser.parse_function_call(arguments), Ok(("", key)) => {
                let arguments = parser.raw_function_arguments(key).to_owned();
                assert_eq!(arguments.len(), expected_values.len());
                for (expected, actual) in expected_values.iter().zip(arguments.to_vec()) {
                    let expected = unwrap_parse(parser.parse_value_like(expected, true, true));
                    assert_eq!(expected, actual);
                }
                check_custom_display(parser.function_arguments(key), displays);
            })
        };
        test_case("()", &[], &["()"]);
        test_case("(123)", &["123"], &["(…)", "(123)"]);
        test_case("(42, 'a')", &["42", "'a'"], &["(…)", "(42, 'a')"]);
    }
}
