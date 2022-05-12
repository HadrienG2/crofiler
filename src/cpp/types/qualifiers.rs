//! Qualifiers that can appear in the type syntax

use crate::cpp::{atoms, IResult};
use nom::Parser;
use nom_supreme::ParserExt;
use std::ops::BitOr;

/// Parser recognizing CV qualifiers
pub fn cv(s: &str) -> IResult<ConstVolatile> {
    use nom::{character::complete::space1, combinator::opt, sequence::preceded};
    let const_ = || {
        atoms::keyword("const").value(ConstVolatile {
            is_const: true,
            is_volatile: false,
        })
    };
    let volatile = || {
        atoms::keyword("volatile").value(ConstVolatile {
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
