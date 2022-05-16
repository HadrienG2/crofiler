//! Atoms from the C++ entity grammar

use crate::cpp::IResult;
use nom::Parser;
use nom_supreme::ParserExt;
use unicode_xid::UnicodeXID;

/// Parser recognizing the end of the input string
pub fn end_of_string(s: &str) -> IResult<()> {
    use nom::combinator::eof;
    eof.value(()).parse(s)
}

/// Parser recognizing the end of an identifier, without consuming it
fn end_of_identifier(s: &str) -> IResult<()> {
    use nom::{character::complete::satisfy, combinator::not};
    (not(satisfy(UnicodeXID::is_xid_continue)).or(end_of_string))
        .peek()
        .parse(s)
}

/// Parser recognizing a C++ keyword
pub fn keyword(word: &'static str) -> impl Fn(&str) -> IResult<()> {
    use nom_supreme::tag::complete::tag;
    move |s| tag(word).and(end_of_identifier).value(()).parse(s)
}

/// Parser recognizing any valid C++ identifier
pub fn identifier(s: &str) -> IResult<&str> {
    use nom::{character::complete::satisfy, multi::many0_count};
    (satisfy(|c| c.is_xid_start() || c == '_')
        .and(many0_count(satisfy(UnicodeXID::is_xid_continue))))
    .recognize()
    .parse(s)
}

/// Parser recognizing C++ integer literals
pub use nom::character::complete::i128 as integer_literal;

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn end_of_string() {
        assert_eq!(super::end_of_string(""), Ok(("", ())));
    }

    #[test]
    fn end_of_identifier() {
        assert_eq!(super::end_of_identifier(""), Ok(("", ())));
        assert_eq!(super::end_of_identifier("+"), Ok(("+", ())));
    }

    #[test]
    fn keyword() {
        assert_eq!(super::keyword("abc")("abc"), Ok(("", ())));
    }

    #[test]
    fn identifier() {
        const ID: &str = "_abcd_1234";
        assert_eq!(super::identifier(ID), Ok(("", ID)));
    }

    #[test]
    fn integer_literal() {
        fn test_integer_literal(num: impl Into<i128>) {
            let num: i128 = num.into();
            let num_str = num.to_string();
            let result: IResult<i128> = super::integer_literal(&num_str);
            assert_eq!(result, Ok(("", num)));
        }
        test_integer_literal(i64::MIN);
        test_integer_literal(u64::MAX);
    }
}
