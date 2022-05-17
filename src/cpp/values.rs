//! Values and other things that follow the value grammar

use crate::cpp::{
    operators::{self, Operator},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing values (and some values that are indistinguishable from
/// values without extra context)
pub fn value_like(s: &str) -> IResult<ValueLike> {
    let integer = integer.map(ValueLike::Integer);
    let character = character.map(ValueLike::Character);
    let unary_op = operators::unary_expr_prefix
        .and(value_like.map(Box::new))
        .map(|(op, expr)| ValueLike::UnaryOp(op, expr));
    // TODO: Should accept id-expressions here, but not necessary yet since they
    //       are accepted by TypeLike as well.
    integer.or(character).or(unary_op).parse(s)
}
//
/// A value, or something that looks close enough to it
#[derive(Clone, Debug, PartialEq)]
pub enum ValueLike<'source> {
    /// Integer
    Integer(i128),

    /// Character
    Character(char),

    /// Unary operator AST
    //
    // TODO: Investigate ways to avoid excessive boxing, something like struct {
    //    prefix: Operator,
    //    inner: ...
    //    calls_and_indices: Box<[...]>
    // }
    // A priori, only values that combine multiple unrelated expressions like
    // BinaryOp and TernaryOp strictly need boxing.
    UnaryOp(Operator<'source>, Box<ValueLike<'source>>),
}
//
// Can't just impl<I: Into<i128>> at it would break other From impls...
macro_rules! value_from_integer {
    ($($integer:ident),*) => {
        $(
            impl From<$integer> for ValueLike<'_> {
                fn from(i: $integer) -> Self {
                    ValueLike::Integer(i.into())
                }
            }
        )*
    }
}
value_from_integer!(i8, u8, i16, u16, i32, u32, i64, u64);
//
impl From<char> for ValueLike<'_> {
    fn from(c: char) -> Self {
        ValueLike::Character(c)
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
    use operators::Symbol;
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
    fn value_like() {
        assert_eq!(super::value_like("-123"), Ok(("", (-123i8).into())));
        assert_eq!(super::value_like("'c'"), Ok(("", 'c'.into())));
        assert_eq!(
            super::value_like("&123"),
            Ok((
                "",
                ValueLike::UnaryOp(Symbol::AndRef.into(), Box::new((123i8).into()))
            ))
        );
    }
}
