//! Function-related parsing

use super::types::{self, TypeLike};
use nom::IResult;

// TODO: Add a function signature parser that parses a parameter list followed
//       by optional cv qualifiers, ref-qualification and noexcept. Expose this
//       publicly and make function_parameters private. Add a test.

/// Parser recognizing a set of function parameters
pub fn function_parameters(s: &str) -> IResult<&str, Box<[TypeLike]>> {
    use nom::{
        character::complete::{char, space0},
        combinator::map,
        multi::separated_list0,
        sequence::{delimited, pair},
    };
    let arguments = separated_list0(pair(char(','), space0), function_parameter);
    let parameters = delimited(char('('), arguments, pair(space0, char(')')));
    map(parameters, |p| p.into_boxed_slice())(s)
}

/// Parser recognizing a single function parameter
fn function_parameter(s: &str) -> IResult<&str, TypeLike> {
    use nom::{
        branch::alt,
        character::complete::{char, space0},
        combinator::map,
        sequence::pair,
    };
    fn delimiter(s: &str) -> IResult<&str, ()> {
        map(pair(space0, alt((char(','), char(')')))), std::mem::drop)(s)
    }
    types::type_like(s, delimiter)
}

#[cfg(test)]
mod tests {
    use super::super::tests::force_parse_type;
    use super::*;

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
