//! Atoms from the C++ grammar

use nom::IResult;
use std::path::Path;
use unicode_xid::UnicodeXID;

/// Parser for C++ identifiers
pub fn identifier(s: &str) -> IResult<&str, &str> {
    use nom::{
        character::complete::satisfy, combinator::recognize, multi::many0_count, sequence::pair,
    };
    recognize(pair(
        satisfy(|c| c.is_xid_start() || c == '_'),
        many0_count(satisfy(UnicodeXID::is_xid_continue)),
    ))(s)
}

/// Parser for C++ integer literals
pub use nom::character::complete::i128 as integer_literal;

/// Parser for CV qualifiers
pub fn cv(s: &str) -> IResult<&str, ConstVolatile> {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::space1,
        combinator::{map, opt},
        sequence::{pair, preceded},
    };
    let const_ = || {
        map(tag("const"), |_| ConstVolatile {
            is_const: true,
            is_volatile: false,
        })
    };
    let volatile = || {
        map(tag("volatile"), |_| ConstVolatile {
            is_const: false,
            is_volatile: true,
        })
    };
    let cv = opt(alt((
        pair(const_(), opt(preceded(space1, volatile()))),
        pair(volatile(), opt(preceded(space1, const_()))),
    )));
    map(cv, |opt_cv| {
        let (cv1, opt_cv2) = opt_cv.unwrap_or_default();
        let cv2 = opt_cv2.unwrap_or_default();
        ConstVolatile {
            is_const: cv1.is_const | cv2.is_const,
            is_volatile: cv1.is_volatile | cv2.is_volatile,
        }
    })(s)
}
//
/// CV qualifiers
#[derive(Default, Debug, PartialEq, Clone, Copy)]
pub struct ConstVolatile {
    is_const: bool,
    is_volatile: bool,
}

/// Parser recognizing primitive types inherited from C, which can have spaces
/// in their name
///
/// This is not a full parser for C++ primitive types, as most of them can be
/// parsed with the regular identifier logic, and we do not need to single out
/// primitives in our processing.
///
/// It will also accept a bunch of types that are invalid from the point of view
/// of the C++ grammar, such as "long long short", for the sake of simplicity:
/// clang should not normally emit these, so we don't really care about
/// processing them right.
pub fn legacy_primitive(s: &str) -> IResult<&str, &str> {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{satisfy, space1},
        combinator::{opt, peek, recognize},
        multi::separated_list1,
        sequence::{pair, terminated},
    };
    let signedness = recognize(pair(opt(tag("un")), tag("signed")));
    let size = alt((tag("short"), tag("long")));
    let base = alt((tag("int"), tag("char"), tag("double")));
    let anything = alt((signedness, size, base));
    terminated(
        recognize(separated_list1(space1, anything)),
        peek(satisfy(|c| !c.is_xid_continue())),
    )(s)
}

/// Parser for clang's <unknown> C++ entity
pub fn unknown_entity(s: &str) -> IResult<&str, ()> {
    use nom::{bytes::complete::tag, combinator::map};
    map(tag("<unknown>"), std::mem::drop)(s)
}

/// Parser for clang lambda types "(lambda at <file path>:<line>:<col>)"
///
/// This will fail if the file path contains a ':' sign other than a
/// Windows-style disk designator at the start, because I have no idea how to
/// handle this inherent grammar ambiguity better...
pub fn lambda(s: &str) -> IResult<&str, Lambda> {
    use nom::{
        bytes::complete::{tag, take_until1},
        character::complete::{anychar, char, u32},
        combinator::{map, opt, recognize},
        sequence::{delimited, pair, separated_pair},
    };
    let disk_designator = recognize(pair(anychar, char(':')));
    let path_str = recognize(pair(opt(disk_designator), take_until1(":")));
    let path = map(path_str, Path::new);
    let location = separated_pair(u32, char(':'), u32);
    let file_location = separated_pair(path, char(':'), location);
    let lambda = map(file_location, |(file, location)| Lambda { file, location });
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

// FIXME: Add tests
