//! Atoms from the C++ entity grammar

use super::IResult;
use nom::Parser;
use nom_supreme::ParserExt;
use std::path::Path;
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
pub fn keyword(word: &'static str) -> impl FnMut(&str) -> IResult<()> + '_ {
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

/// Parser for clang's <unknown> C++ entity
pub fn unknown_entity(s: &str) -> IResult<()> {
    use nom_supreme::tag::complete::tag;
    tag("<unknown>").value(()).parse(s)
}

/// Parser for clang lambda types "(lambda at <file path>:<line>:<col>)"
///
/// This will fail if the file path contains a ':' sign other than a
/// Windows-style disk designator at the start, because I have no idea how to
/// handle this inherent grammar ambiguity better...
pub fn lambda(s: &str) -> IResult<Lambda> {
    use nom::{
        bytes::complete::{tag, take_until1},
        character::complete::{anychar, char, u32},
        combinator::{opt, recognize},
        sequence::{delimited, separated_pair},
    };

    let location = separated_pair(u32, char(':'), u32);

    let disk_designator = anychar.and(char(':'));
    let path_str = recognize(opt(disk_designator).and(take_until1(":")));
    let path = path_str.map(Path::new);

    let file_location = separated_pair(path, char(':'), location);
    let lambda = file_location.map(|(file, location)| Lambda { file, location });
    delimited(tag("(lambda at "), lambda, char(')'))(s)
}
//
/// Lambda location description
#[derive(Clone, Debug, PartialEq)]
pub struct Lambda<'source> {
    /// In which file the lambda is declared
    file: &'source Path,

    /// Where exactly in the file
    location: (Line, Col),
}
//
type Line = u32;
type Col = u32;

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

    #[test]
    fn unknown_entity() {
        assert_eq!(super::unknown_entity("<unknown>"), Ok(("", ())));
    }

    #[test]
    fn lambda() {
        assert_eq!(
            super::lambda("(lambda at /path/to/source.cpp:123:45)"),
            Ok((
                "",
                Lambda {
                    file: Path::new("/path/to/source.cpp"),
                    location: (123, 45)
                }
            ))
        );
    }
}
