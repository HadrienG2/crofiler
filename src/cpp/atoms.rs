//! Atoms from the C++ entity grammar

use super::IResult;
use nom::Parser;
use nom_supreme::ParserExt;
use std::{ops::BitOr, path::Path};
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

/// Parser recognizing CV qualifiers
pub fn cv(s: &str) -> IResult<ConstVolatile> {
    use nom::{character::complete::space1, combinator::opt, sequence::preceded};
    let const_ = || {
        keyword("const").value(ConstVolatile {
            is_const: true,
            is_volatile: false,
        })
    };
    let volatile = || {
        keyword("volatile").value(ConstVolatile {
            is_const: false,
            is_volatile: true,
        })
    };
    opt((const_().and(opt(preceded(space1, volatile()))))
        .or(volatile().and(opt(preceded(space1, const_())))))
    .map(|opt_cv| {
        let (cv1, opt_cv2) = opt_cv.unwrap_or_default();
        let cv2 = opt_cv2.unwrap_or_default();
        cv1 | cv2
    })
    .parse(s)
}
//
/// CV qualifiers
#[derive(Default, Debug, PartialEq, Clone, Copy)]
pub struct ConstVolatile {
    /// Const qualifier
    is_const: bool,

    /// Volatile qualifier
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

/// Parser recognizing reference qualifiers
pub fn reference(s: &str) -> IResult<Reference> {
    use nom::{character::complete::char, combinator::map_opt, multi::many0_count};
    let num_refs = many0_count(char('&'));
    map_opt(num_refs, |num| match num {
        0 => Some(Reference::None),
        1 => Some(Reference::LValue),
        2 => Some(Reference::RValue),
        _ => None,
    })(s)
}
//
/// Reference signs
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reference {
    /// No reference signs
    None,

    /// lvalue reference = 1 reference sign
    LValue,

    /// rvalue reference = 2 reference signs
    RValue,
}
//
impl Default for Reference {
    fn default() -> Self {
        Self::None
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
pub fn legacy_primitive(s: &str) -> IResult<&str> {
    use nom::{character::complete::space1, combinator::opt, multi::many0_count};
    use nom_supreme::tag::complete::tag;

    fn anything(s: &str) -> IResult<()> {
        let signedness = opt(tag("un")).and(keyword("signed")).value(());
        let size = keyword("short").or(keyword("long"));
        let base = keyword("int").or(keyword("char")).or(keyword("double"));
        signedness.or(size).or(base).parse(s)
    }

    // This is an allocation-free alternative to
    // separated_list1(space1, anything).recognize()
    (anything.and(many0_count(space1.and(anything))))
        .recognize()
        .parse(s)
}

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
    fn cv() {
        assert_eq!(super::cv(""), Ok(("", ConstVolatile::default())));
        assert_eq!(super::cv("const"), Ok(("", ConstVolatile::CONST)));
        assert_eq!(super::cv("volatile"), Ok(("", ConstVolatile::VOLATILE)));
        let const_volatile = ConstVolatile::CONST | ConstVolatile::VOLATILE;
        assert_eq!(super::cv("const volatile"), Ok(("", const_volatile)));
        assert_eq!(super::cv("volatile const"), Ok(("", const_volatile)));
    }

    #[test]
    fn reference() {
        assert_eq!(super::reference(""), Ok(("", Reference::None)));
        assert_eq!(super::reference("&"), Ok(("", Reference::LValue)));
        assert_eq!(super::reference("&&"), Ok(("", Reference::RValue)));
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
