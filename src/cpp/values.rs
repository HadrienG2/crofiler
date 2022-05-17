//! Values and other things that follow the value grammar

use crate::cpp::IResult;
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser recognizing values (and some values that are indistinguishable from
/// values without extra context)
pub fn value_like(s: &str) -> IResult<ValueLike> {
    integer(s)
}
//
/// A value, or something that looks close enough to it
pub type ValueLike = i128;

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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

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
}
