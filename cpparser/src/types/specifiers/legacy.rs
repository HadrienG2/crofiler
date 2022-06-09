//! Handling of legacy C-style type names with inner spaces
//!
//! See <https://en.cppreference.com/w/cpp/language/types> for context.

use crate::{EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;

impl EntityParser {
    /// Parser for legacy C-style type specifiers that can have spaces in them
    ///
    /// This only parses C primitive type names that do have a space in their
    /// name, others can be handled just fine by the regular IdExpression logic.
    ///
    pub fn parse_legacy_name<'source>(&self, s: &'source str) -> IResult<'source, LegacyName> {
        (self.legacy_name_parser)(s)
    }
}

/// Generate a parser for legacy C-style type specifiers that can have spaces
///
/// See EntityParser::parse_legacy_name for semantics.
///
#[inline(always)]
pub(crate) fn legacy_name_parser() -> impl Fn(&str) -> IResult<LegacyName> {
    use nom::{character::complete::space0, multi::fold_many1};

    // Parser for keywords that can appear in legacy names
    let keyword = EntityParser::keywords_parser([
        (
            "double",
            LegacyNameBuilder {
                base: Some(Base::Double),
                signedness: None,
                size: None,
            },
        ),
        (
            "long",
            LegacyNameBuilder {
                base: None,
                signedness: None,
                size: Some(Size::Long),
            },
        ),
        (
            "unsigned",
            LegacyNameBuilder {
                base: None,
                signedness: Some(Signedness::Unsigned),
                size: None,
            },
        ),
        (
            "char",
            LegacyNameBuilder {
                base: Some(Base::Char),
                signedness: None,
                size: None,
            },
        ),
        (
            "int",
            LegacyNameBuilder {
                base: Some(Base::Int),
                signedness: None,
                size: None,
            },
        ),
        (
            "signed",
            LegacyNameBuilder {
                base: None,
                signedness: Some(Signedness::Signed),
                size: None,
            },
        ),
        (
            "short",
            LegacyNameBuilder {
                base: None,
                signedness: None,
                size: Some(Size::Short),
            },
        ),
    ]);

    // Parser for legacy names based on those keywords
    move |s| {
        fold_many1(
            (&keyword).terminated(space0),
            LegacyNameBuilder::default,
            |mut acc, item| {
                assert!(
                    acc.base.is_none() || item.base.is_none(),
                    "Incompatible base qualifiers {:?} and {:?}",
                    acc.base,
                    item.base
                );
                acc.base = acc.base.or(item.base);

                assert!(
                    acc.signedness.is_none() || item.signedness.is_none(),
                    "Incompatible signedness qualifiers {:?} and {:?}",
                    acc.signedness,
                    item.signedness
                );
                acc.signedness = acc.signedness.or(item.signedness);

                if let (Some(Size::Long), Some(Size::Long)) = (acc.size, item.size) {
                    acc.size = Some(Size::LongLong);
                } else {
                    assert!(
                        acc.size.is_none() || item.size.is_none(),
                        "Incompatible size qualifiers {:?} and {:?}",
                        acc.size,
                        item.size
                    );
                    acc.size = acc.size.or(item.size);
                }
                acc
            },
        )
        .map(LegacyNameBuilder::build)
        .parse(s)
    }
}

/// C-style type
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum LegacyName {
    /// "short int"
    SignedShort,

    /// "unsigned short int"
    UnsignedShort,

    /// "int"
    SignedInt,

    /// "unsigned int"
    UnsignedInt,

    /// "long int"
    SignedLong,

    /// "unsigned long int"
    UnsignedLong,

    /// "long long int"
    SignedLongLong,

    /// "unsigned long long int"
    UnsignedLongLong,

    /// "char"
    Char,

    /// "signed char"
    SignedChar,

    /// "unsigned char"
    UnsignedChar,

    /// "double"
    Double,

    /// "long double"
    LongDouble,
}

/// C-style type name component
#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
struct LegacyNameBuilder {
    /// Base type (usually int)
    base: Option<Base>,

    /// Whether the type is unsigned, signed, or of implementation-defined signedness
    signedness: Option<Signedness>,

    /// How long the type should be
    size: Option<Size>,
}
//
impl LegacyNameBuilder {
    fn build(self) -> LegacyName {
        match self {
            LegacyNameBuilder {
                base: Some(Base::Double),
                signedness: None,
                size: None,
            } => LegacyName::Double,

            LegacyNameBuilder {
                base: Some(Base::Double),
                signedness: None,
                size: Some(Size::Long),
            } => LegacyName::LongDouble,

            LegacyNameBuilder {
                base: Some(Base::Double),
                signedness,
                size,
            } => panic!(
                "Invalid size or signedness qualifier (resp. {size:?} and {signedness:?}) on type double"
            ),

            // From here, base can't be Double ===

            LegacyNameBuilder {
                base: Some(Base::Char),
                signedness: None,
                size: None,
            } => LegacyName::Char,

            LegacyNameBuilder {
                base: Some(Base::Char),
                signedness: Some(Signedness::Signed),
                size: None,
            } => LegacyName::SignedChar,

            LegacyNameBuilder {
                base: Some(Base::Char),
                signedness: Some(Signedness::Unsigned),
                size: None,
            } => LegacyName::UnsignedChar,

            LegacyNameBuilder {
                base: Some(Base::Char),
                size: Some(sz),
                ..
            } => panic!("Invalid size qualifier {sz:?} on type char"),

            // From here, base can't be Char or Double, so it has to be Int

            LegacyNameBuilder {
                base: _,
                signedness: Some(Signedness::Unsigned),
                size: None,
            } => LegacyName::UnsignedInt,

            LegacyNameBuilder {
                base: _,
                signedness: Some(Signedness::Unsigned),
                size: Some(Size::Short),
            } => LegacyName::UnsignedShort,

            LegacyNameBuilder {
                base: _,
                signedness: Some(Signedness::Unsigned),
                size: Some(Size::Long),
            } => LegacyName::UnsignedLong,

            LegacyNameBuilder {
                base: _,
                signedness: Some(Signedness::Unsigned),
                size: Some(Size::LongLong),
            } => LegacyName::UnsignedLongLong,

            // From here, unsigned integers are done, only signed ones remain

            LegacyNameBuilder {
                base: None,
                signedness: None,
                size: None,
            } => unreachable!("Should be forbidden by fold_many1"),

            LegacyNameBuilder {
                base: _,
                signedness: _,
                size: Some(Size::Short),
            } => LegacyName::SignedShort,

            LegacyNameBuilder {
                base: _,
                signedness: _,
                size: None,
            } => LegacyName::SignedInt,

            LegacyNameBuilder {
                base: _,
                signedness: _,
                size: Some(Size::Long),
            } => LegacyName::SignedLong,

            LegacyNameBuilder {
                base: _,
                signedness: _,
                size: Some(Size::LongLong),
            } => LegacyName::SignedLongLong,
        }
    }
}

/// C-style type signedness
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
enum Signedness {
    /// Most types are signed by default
    Signed,

    /// The "unsigned" keyword can be used to force a type to be unsigned
    Unsigned,
}
//
/// C-style type size
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
enum Size {
    /// Short is usually 16-bit
    Short,

    /// Long is 64-bit on most 64-bit systems except on Windows/MSVC (32-bit)
    Long,

    /// LongLong is always 64-bit, and incompatible with Long because that's C
    LongLong,
}
//
/// C-style base type to which qualifiers can be applied
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
enum Base {
    /// "int" (default if unspecified)
    Int,

    /// "char" (usually a byte)
    Char,

    /// "double" (usually IEEE-754 binary64)
    Double,
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn legacy_name() {
        use LegacyName::*;
        let parser = EntityParser::new();
        let test_legacy_name = |i, o| assert_eq!(parser.parse_legacy_name(i), Ok(("", o)));

        test_legacy_name("short", SignedShort);
        test_legacy_name("short int", SignedShort);
        test_legacy_name("signed short", SignedShort);
        test_legacy_name("signed short int", SignedShort);

        test_legacy_name("unsigned short", UnsignedShort);
        test_legacy_name("unsigned short int", UnsignedShort);

        test_legacy_name("int", SignedInt);
        test_legacy_name("signed", SignedInt);
        test_legacy_name("signed int", SignedInt);

        test_legacy_name("unsigned", UnsignedInt);
        test_legacy_name("unsigned int", UnsignedInt);

        test_legacy_name("long", SignedLong);
        test_legacy_name("long int", SignedLong);
        test_legacy_name("signed long", SignedLong);
        test_legacy_name("signed long int", SignedLong);

        test_legacy_name("unsigned long", UnsignedLong);
        test_legacy_name("unsigned long int", UnsignedLong);

        test_legacy_name("long long", SignedLongLong);
        test_legacy_name("long long int", SignedLongLong);
        test_legacy_name("signed long long", SignedLongLong);
        test_legacy_name("signed long long int", SignedLongLong);

        test_legacy_name("unsigned long long", UnsignedLongLong);
        test_legacy_name("unsigned long long int", UnsignedLongLong);
        test_legacy_name("long int unsigned long", UnsignedLongLong);

        test_legacy_name("char", Char);
        test_legacy_name("signed char", SignedChar);
        test_legacy_name("unsigned char", UnsignedChar);

        test_legacy_name("double", Double);
        test_legacy_name("long double", LongDouble);
    }
}
