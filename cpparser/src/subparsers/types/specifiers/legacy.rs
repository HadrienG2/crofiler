//! Handling of legacy C-style type names with inner spaces
//!
//! See <https://en.cppreference.com/w/cpp/language/types> for context.

use crate::{EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;
use std::fmt::{self, Display, Formatter};
use thiserror::Error;

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
#[inline]
pub(crate) fn legacy_name_parser() -> impl Fn(&str) -> IResult<LegacyName> {
    use nom::{character::complete::multispace0, multi::fold_many1};

    // Parser for keywords that can appear in legacy names
    let keyword = EntityParser::keywords_parser([
        (
            "float",
            LegacyNameBuilder {
                base: Some(Base::Float),
                ..Default::default()
            },
        ),
        (
            "double",
            LegacyNameBuilder {
                base: Some(Base::Double),
                ..Default::default()
            },
        ),
        (
            "long",
            LegacyNameBuilder {
                size: Some(Size::Long),
                ..Default::default()
            },
        ),
        (
            "unsigned",
            LegacyNameBuilder {
                signedness: Some(Signedness::Unsigned),
                ..Default::default()
            },
        ),
        (
            "char",
            LegacyNameBuilder {
                base: Some(Base::Char),
                ..Default::default()
            },
        ),
        (
            "int",
            LegacyNameBuilder {
                base: Some(Base::Int),
                ..Default::default()
            },
        ),
        (
            "__int128",
            LegacyNameBuilder {
                base: Some(Base::Int128),
                ..Default::default()
            },
        ),
        (
            "signed",
            LegacyNameBuilder {
                signedness: Some(Signedness::Signed),
                ..Default::default()
            },
        ),
        (
            "short",
            LegacyNameBuilder {
                size: Some(Size::Short),
                ..Default::default()
            },
        ),
        (
            "_Complex",
            LegacyNameBuilder {
                complex: true,
                ..Default::default()
            },
        ),
    ]);

    // Parser for legacy names based on those keywords
    move |s| {
        fold_many1(
            (&keyword).terminated(multispace0),
            || Ok(LegacyNameBuilder::default()),
            |acc, item| {
                let mut acc = acc?;

                if let (Some(base1), Some(base2)) = (acc.base, item.base) {
                    return Err(ParseError::IncompatibleBase(base1, base2));
                }
                acc.base = acc.base.or(item.base);

                if let (Some(sign1), Some(sign2)) = (acc.signedness, item.signedness) {
                    return Err(ParseError::IncompatibleSignedness(sign1, sign2));
                }
                acc.signedness = acc.signedness.or(item.signedness);

                if acc.complex && item.complex {
                    return Err(ParseError::DuplicateComplex);
                }
                acc.complex |= item.complex;

                match (acc.size, item.size) {
                    (Some(Size::Long), Some(Size::Long)) => acc.size = Some(Size::LongLong),
                    (Some(size1), Some(size2)) => {
                        return Err(ParseError::IncompatibleSizes(size1, size2))
                    }
                    _ => acc.size = acc.size.or(item.size),
                }

                Ok(acc)
            },
        )
        .map_res(|x| x)
        .map_res(LegacyNameBuilder::build)
        .parse(s)
    }
}

/// C-style type
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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

    /// "__int128"
    SignedInt128,

    /// "unsigned __int128"
    UnsignedInt128,

    /// "char"
    Char,

    /// "signed char"
    SignedChar,

    /// "unsigned char"
    UnsignedChar,

    /// "float"
    Float,

    /// "double"
    Double,

    /// "long double"
    LongDouble,

    /// "float _Complex"
    FloatComplex,

    /// "double _Complex"
    DoubleComplex,

    /// "long double _Complex"
    LongDoubleComplex,
}
//
impl Display for LegacyName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        let s = match self {
            Self::SignedShort => "short",
            Self::UnsignedShort => "unsigned short",
            Self::SignedInt => "int",
            Self::UnsignedInt => "unsigned int",
            Self::SignedLong => "long",
            Self::UnsignedLong => "unsigned long",
            Self::SignedLongLong => "long long",
            Self::UnsignedLongLong => "unsigned long long",
            Self::SignedInt128 => "__int128",
            Self::UnsignedInt128 => "unsigned __int128",
            Self::Char => "char",
            Self::SignedChar => "signed char",
            Self::UnsignedChar => "unsigned char",
            Self::Float => "float",
            Self::Double => "double",
            Self::LongDouble => "long double",
            Self::FloatComplex => "float _Complex",
            Self::DoubleComplex => "double _Complex",
            Self::LongDoubleComplex => "long double _Complex",
        };
        write!(f, "{s}")
    }
}

/// C-style type name component
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct LegacyNameBuilder {
    /// Base type (usually int)
    base: Option<Base>,

    /// Whether the type is unsigned, signed, or of implementation-defined signedness
    signedness: Option<Signedness>,

    /// How long the type should be
    size: Option<Size>,

    /// Whether this is a C-style complex number
    complex: bool,
}
//
impl LegacyNameBuilder {
    fn build(self) -> Result<LegacyName, ParseError> {
        let res = match self {
            LegacyNameBuilder {
                base: Some(Base::Float),
                signedness: None,
                size: None,
                complex,
            } => {
                if complex {
                    LegacyName::FloatComplex
                } else {
                    LegacyName::Float
                }
            }

            LegacyNameBuilder {
                base: Some(Base::Float),
                signedness,
                size,
                complex: _,
            } => {
                return Err(ParseError::IncompatibleBaseSizeSignedness(
                    Base::Float,
                    size,
                    signedness,
                ))
            }

            LegacyNameBuilder {
                base: Some(Base::Double),
                signedness: None,
                size: None,
                complex,
            } => {
                if complex {
                    LegacyName::DoubleComplex
                } else {
                    LegacyName::Double
                }
            }

            LegacyNameBuilder {
                base: Some(Base::Double),
                signedness: None,
                size: Some(Size::Long),
                complex,
            } => {
                if complex {
                    LegacyName::LongDoubleComplex
                } else {
                    LegacyName::LongDouble
                }
            }

            LegacyNameBuilder {
                base: Some(Base::Double),
                signedness,
                size,
                complex: _,
            } => {
                return Err(ParseError::IncompatibleBaseSizeSignedness(
                    Base::Double,
                    size,
                    signedness,
                ))
            }

            // From here, base can't be a floating-point type ===
            LegacyNameBuilder { complex: true, .. } => {
                return Err(ParseError::IncompatibleComplexInt(self.base))
            }

            LegacyNameBuilder {
                base: Some(Base::Char),
                signedness: None,
                size: None,
                complex: false,
            } => LegacyName::Char,

            LegacyNameBuilder {
                base: Some(Base::Char),
                signedness: Some(Signedness::Signed),
                size: None,
                complex: false,
            } => LegacyName::SignedChar,

            LegacyNameBuilder {
                base: Some(Base::Char),
                signedness: Some(Signedness::Unsigned),
                size: None,
                complex: false,
            } => LegacyName::UnsignedChar,

            LegacyNameBuilder {
                base: Some(Base::Char),
                size: Some(sz),
                ..
            } => return Err(ParseError::IncompatibleBaseSize(Base::Char, sz)),

            LegacyNameBuilder {
                base: Some(Base::Int128),
                signedness: None,
                size: None,
                complex: false,
            } => LegacyName::SignedInt128,

            LegacyNameBuilder {
                base: Some(Base::Int128),
                signedness: Some(Signedness::Signed),
                size: None,
                complex: false,
            } => LegacyName::SignedInt128,

            LegacyNameBuilder {
                base: Some(Base::Int128),
                signedness: Some(Signedness::Unsigned),
                size: None,
                complex: false,
            } => LegacyName::UnsignedInt128,

            LegacyNameBuilder {
                base: Some(Base::Int128),
                size: Some(sz),
                ..
            } => return Err(ParseError::IncompatibleBaseSize(Base::Int128, sz)),

            // From here, base can't be Char, __int128 or floating-point, so it has to be Int
            LegacyNameBuilder {
                signedness: Some(Signedness::Unsigned),
                size: None,
                ..
            } => LegacyName::UnsignedInt,

            LegacyNameBuilder {
                signedness: Some(Signedness::Unsigned),
                size: Some(Size::Short),
                ..
            } => LegacyName::UnsignedShort,

            LegacyNameBuilder {
                signedness: Some(Signedness::Unsigned),
                size: Some(Size::Long),
                ..
            } => LegacyName::UnsignedLong,

            LegacyNameBuilder {
                signedness: Some(Signedness::Unsigned),
                size: Some(Size::LongLong),
                ..
            } => LegacyName::UnsignedLongLong,

            // From here, unsigned integers are done, only signed ones remain
            LegacyNameBuilder {
                base: None,
                signedness: None,
                size: None,
                complex: false,
            } => unreachable!("Should be forbidden by fold_many1"),

            LegacyNameBuilder {
                size: Some(Size::Short),
                ..
            } => LegacyName::SignedShort,

            LegacyNameBuilder { size: None, .. } => LegacyName::SignedInt,

            LegacyNameBuilder {
                size: Some(Size::Long),
                ..
            } => LegacyName::SignedLong,

            LegacyNameBuilder {
                size: Some(Size::LongLong),
                ..
            } => LegacyName::SignedLongLong,
        };
        Ok(res)
    }
}

/// C-style type signedness
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
enum Signedness {
    /// Most types are signed by default
    Signed,

    /// The "unsigned" keyword can be used to force a type to be unsigned
    Unsigned,
}
//
/// C-style type size
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
enum Base {
    /// "int" (default if unspecified)
    Int,

    /// "char" (usually a byte)
    Char,

    /// "__int128" (compiler extension)
    Int128,

    /// "float" (usually IEEE-754 binary32)
    Float,

    /// "double" (usually IEEE-754 binary64)
    Double,
}

/// Errors that can occur while parsing legacy types
#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
enum ParseError {
    /// Incompatible base types were specified
    #[error("incompatible base types: {0:?} and {1:?}")]
    IncompatibleBase(Base, Base),

    /// Incompatible type sizes were specified
    #[error("incompatible sizes: {0:?} and {1:?}")]
    IncompatibleSizes(Size, Size),

    /// Incompatible signednesses were specified
    #[error("incompatible signednesses: {0:?} and {1:?}")]
    IncompatibleSignedness(Signedness, Signedness),

    /// Redundant _Complex qualifier
    #[error("invalid repetition of _Complex qualifier")]
    DuplicateComplex,

    /// Bad qualifiers for a certain base type
    #[error(
        "incompatible size ({1:?}) and/or signedness ({2:?}) qualifier(s) for base type {1:?}"
    )]
    IncompatibleBaseSizeSignedness(Base, Option<Size>, Option<Signedness>),

    /// Applied _Complex qualifier to a non-floating-point type
    #[error("_Complex qualifier is invalid for non-floating-point base type {0:?}")]
    IncompatibleComplexInt(Option<Base>),

    /// Applied a size qualifier to a non-resizable type
    #[error("Applied size qualifier {1:?} to non-resizable base {0:?}")]
    IncompatibleBaseSize(Base, Size),
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

        test_legacy_name("__int128", SignedInt128);
        test_legacy_name("signed __int128", SignedInt128);

        test_legacy_name("unsigned __int128", UnsignedInt128);

        test_legacy_name("char", Char);
        test_legacy_name("signed char", SignedChar);
        test_legacy_name("unsigned char", UnsignedChar);

        test_legacy_name("float", Float);
        test_legacy_name("double", Double);
        test_legacy_name("long double", LongDouble);

        test_legacy_name("float _Complex", FloatComplex);
        test_legacy_name("double _Complex", DoubleComplex);
        test_legacy_name("long double _Complex", LongDoubleComplex);
    }
}
