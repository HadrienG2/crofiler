//! Literals (and things that should be literals like negative numbers)

use crate::cpp::{atoms, IResult};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser for literals
pub fn literal(s: &str) -> IResult<Literal> {
    use nom::combinator::opt;
    (literal_value.and(opt(atoms::identifier)))
        .map(|(value, custom_suffix)| Literal {
            value,
            custom_suffix,
        })
        .parse(s)
}

/// A modern C++ literal, accounting for custom literals
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Literal<'source> {
    /// Inner value
    value: LiteralValue,

    /// Custom literal suffix, if any
    custom_suffix: Option<&'source str>,
}
//
impl<T: Into<LiteralValue>> From<T> for Literal<'_> {
    fn from(value: T) -> Self {
        Self {
            value: value.into(),
            custom_suffix: None,
        }
    }
}

/// Parser for literal values
fn literal_value(s: &str) -> IResult<LiteralValue> {
    let integer = integer.map(LiteralValue::Integer);
    let character = character.map(LiteralValue::Character);
    integer.or(character).parse(s)
}

/// A literal value, or something that looks close enough to it
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralValue {
    /// Integer
    Integer(i128),

    /// Character
    Character(char),
}
//
// Can't just impl<I: Into<i128>> at it would break other From impls...
macro_rules! literal_value_from_integer {
    ($($integer:ident),*) => {
        $(
            impl From<$integer> for LiteralValue {
                fn from(i: $integer) -> Self {
                    LiteralValue::Integer(i.into())
                }
            }
        )*
    }
}
literal_value_from_integer!(i8, u8, i16, u16, i32, u32, i64, u64);
//
impl From<char> for LiteralValue {
    fn from(c: char) -> Self {
        LiteralValue::Character(c)
    }
}

/// Parser recognizing C-style integer literals + negative numbers
fn integer(s: &str) -> IResult<i128> {
    use nom::{
        character::complete::{i128, satisfy},
        multi::many0_count,
    };
    i128.terminated(many0_count(satisfy(|c| {
        let c = c.to_ascii_uppercase();
        c == 'U' || c == 'L' || c == 'Z'
    })))
    .parse(s)
}

/// Parser recognizing C-style character literals
fn character(s: &str) -> IResult<char> {
    use nom::{
        character::complete::{anychar, char},
        combinator::opt,
        sequence::{delimited, preceded},
    };
    use nom_supreme::tag::complete::tag;
    let prefix = opt(tag("u8")
        .value('8')
        .or(char('u'))
        .or(char('U'))
        .or(char('L')));
    let escape_sequence = (char('t').value('\t'))
        .or(char('r').value('\r'))
        .or(char('n').value('\n'))
        .or(char('\''))
        .or(char('"'))
        .or(char('\\'));
    delimited(
        prefix.and(char('\'')),
        preceded(char('\\'), escape_sequence).or(anychar),
        char('\''),
    )(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::fmt::Write;

    #[test]
    fn integer() {
        fn test_integer(num: impl Into<i128>) {
            let num: i128 = num.into();
            let num_str = num.to_string();
            for unsigned_suffix in ["", "U"] {
                for size_suffix in ["", "L", "LL", "Z"] {
                    for lowercase in [false, true] {
                        for size_first in [false, true] {
                            let suffix = if size_first {
                                size_suffix.chars().chain(unsigned_suffix.chars())
                            } else {
                                unsigned_suffix.chars().chain(size_suffix.chars())
                            };
                            let case = |c: char| {
                                if lowercase {
                                    c.to_ascii_lowercase()
                                } else {
                                    c
                                }
                            };
                            let mut num_str = num_str.clone();
                            for c in suffix.map(case) {
                                num_str.push(c);
                            }
                            let result: IResult<i128> = super::integer(&num_str);
                            assert_eq!(result, Ok(("", num)));
                        }
                    }
                }
            }
        }
        test_integer(i64::MIN);
        test_integer(u64::MAX);
    }

    #[test]
    fn character() {
        fn test_character_str(c: char) {
            for prefix in ["", "u8", "u", "U", "L"] {
                let mut char_str = prefix.to_string();
                write!(&mut char_str, "'{}'", c.escape_default()).unwrap();
                println!("{char_str}");
                assert_eq!(super::character(&char_str), Ok(("", c)));
            }
        }
        test_character_str('x');
        test_character_str('\t');
        test_character_str('\n');
        test_character_str('\'');
    }

    #[test]
    fn literal_value() {
        assert_eq!(super::literal_value("-123"), Ok(("", (-123i8).into())));
        assert_eq!(super::literal_value("'c'"), Ok(("", 'c'.into())));
    }

    #[test]
    fn literal() {
        assert_eq!(super::literal("'x'"), Ok(("", 'x'.into())));
        assert_eq!(
            super::literal("42_m"),
            Ok((
                "",
                Literal {
                    value: 42u8.into(),
                    custom_suffix: Some("_m")
                }
            ))
        );
    }
}
