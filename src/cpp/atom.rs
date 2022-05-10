//! Atoms from the C++ grammar

use nom::IResult;
use std::{ops::BitOr, path::Path};
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
        cv1 | cv2
    })(s)
}
//
/// CV qualifiers
#[derive(Default, Debug, PartialEq, Clone, Copy)]
pub struct ConstVolatile {
    is_const: bool,
    is_volatile: bool,
}
//
impl ConstVolatile {
    /// Lone const qualifier
    pub const CONST: ConstVolatile = ConstVolatile {
        is_const: true,
        is_volatile: false,
    };

    /// Lone volatile qualifier
    pub const VOLATILE: ConstVolatile = ConstVolatile {
        is_const: false,
        is_volatile: true,
    };
}
//
impl BitOr for ConstVolatile {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self {
            is_const: self.is_const | rhs.is_const,
            is_volatile: self.is_volatile | rhs.is_volatile,
        }
    }
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
        combinator::{eof, map, not, opt, peek, recognize},
        multi::separated_list1,
        sequence::{pair, terminated},
    };
    let signedness = recognize(pair(opt(tag("un")), tag("signed")));
    let size = alt((tag("short"), tag("long")));
    let base = alt((tag("int"), tag("char"), tag("double")));
    let anything = alt((signedness, size, base));
    terminated(
        recognize(separated_list1(space1, anything)),
        peek(alt((
            map(eof, std::mem::drop),
            not(satisfy(UnicodeXID::is_xid_continue)),
        ))),
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

#[cfg(test)]
mod tests {
    use super::*;

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
            let result: IResult<&str, i128> = super::integer_literal(&num_str);
            assert_eq!(result, Ok(("", num)));
        }
        test_integer_literal(i64::MAX);
        test_integer_literal(u64::MAX);
    }

    #[test]
    fn cv() {
        assert_eq!(super::cv(""), Ok(("", ConstVolatile::default())));
        assert_eq!(super::cv("const"), Ok(("", ConstVolatile::CONST)));
        assert_eq!(super::cv("volatile"), Ok(("", ConstVolatile::VOLATILE)));
        let const_volatile = ConstVolatile::CONST | ConstVolatile::VOLATILE;
        assert_eq!(super::cv("const volatile"), Ok(("", const_volatile)));
        assert_eq!(super::cv("volatile const"), Ok(("", const_volatile)));
    }

    #[test]
    fn legacy_primitive() {
        let test_legacy_primitive = |s| assert_eq!(super::legacy_primitive(s), Ok(("", s)));

        test_legacy_primitive("short int");
        test_legacy_primitive("unsigned short int");

        test_legacy_primitive("int");
        test_legacy_primitive("unsigned int");

        test_legacy_primitive("long int");
        test_legacy_primitive("unsigned long int");

        test_legacy_primitive("long long int");
        test_legacy_primitive("unsigned long long int");
        test_legacy_primitive("long int unsigned long");

        test_legacy_primitive("char");
        test_legacy_primitive("signed char");
        test_legacy_primitive("unsigned char");

        test_legacy_primitive("double");
        test_legacy_primitive("long double");
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
