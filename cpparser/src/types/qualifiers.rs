//! Qualifiers that can appear in the type syntax

use crate::{EntityParser, IResult};
use nom_supreme::ParserExt;
use std::ops::BitOr;

impl EntityParser {
    /// Parser recognizing CV qualifiers
    pub fn parse_cv(s: &str) -> IResult<ConstVolatile> {
        use nom::{character::complete::space0, multi::fold_many0};
        let keyword = Self::keywords_parser([
            ("const", ConstVolatile::CONST),
            ("volatile", ConstVolatile::VOLATILE),
        ]);
        fold_many0(
            keyword.terminated(space0),
            ConstVolatile::default,
            |acc, cv| acc | cv,
        )(s)
    }

    /// Parser recognizing reference qualifiers
    pub fn parse_reference(s: &str) -> IResult<Reference> {
        use nom::{character::complete::char, combinator::map_opt, multi::many0_count};
        let num_refs = many0_count(char('&'));
        map_opt(num_refs, |num| match num {
            0 => Some(Reference::None),
            1 => Some(Reference::LValue),
            2 => Some(Reference::RValue),
            _ => None,
        })(s)
    }
}

/// CV qualifiers
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
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

/// Reference qualifiers
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn cv() {
        assert_eq!(
            EntityParser::parse_cv(""),
            Ok(("", ConstVolatile::default()))
        );
        assert_eq!(
            EntityParser::parse_cv("const"),
            Ok(("", ConstVolatile::CONST))
        );
        assert_eq!(
            EntityParser::parse_cv("volatile"),
            Ok(("", ConstVolatile::VOLATILE))
        );
        let const_volatile = ConstVolatile::CONST | ConstVolatile::VOLATILE;
        assert_eq!(
            EntityParser::parse_cv("const volatile"),
            Ok(("", const_volatile))
        );
        assert_eq!(
            EntityParser::parse_cv("volatile const"),
            Ok(("", const_volatile))
        );

        // This is invalid per C++ grammar, but clang will emit it anyway
        assert_eq!(
            EntityParser::parse_cv("const const"),
            Ok(("", ConstVolatile::CONST))
        );
    }

    #[test]
    fn reference() {
        assert_eq!(EntityParser::parse_reference(""), Ok(("", Reference::None)));
        assert_eq!(
            EntityParser::parse_reference("&"),
            Ok(("", Reference::LValue))
        );
        assert_eq!(
            EntityParser::parse_reference("&&"),
            Ok(("", Reference::RValue))
        );
    }
}
