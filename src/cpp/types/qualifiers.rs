//! Qualifiers that can appear in the type syntax

use crate::cpp::{
    atoms,
    names::{self, NestedNameSpecifier},
    IResult,
};
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

/// Parser recognizing pointer + reference qualifiers
pub fn pointers_reference(s: &str) -> IResult<PointersReference> {
    use nom::{character::complete::space0, multi::many0, sequence::preceded};
    let pointers = many0(pointer).map(Vec::into_boxed_slice);
    let reference = preceded(space0, reference);
    (pointers.and(reference))
        .map(|(pointers, reference)| PointersReference {
            pointers,
            reference,
        })
        .parse(s)
}
//
/// Pointer and reference qualifiers
#[derive(Debug, Default, PartialEq, Clone)]
pub struct PointersReference<'source> {
    /// Layers of pointer indirection (* const * volatile...)
    pointers: Box<[Pointer<'source>]>,

    /// Reference qualifiers
    reference: Reference,
}

/// Parser recognizing a pointer declaration
fn pointer(s: &str) -> IResult<Pointer> {
    use nom::{
        character::complete::{char, space0},
        sequence::delimited,
    };
    let nested_star = names::nested_name_specifier.terminated(char('*'));
    (delimited(space0, nested_star, space0).and(cv))
        .map(|(path, cv)| Pointer { path, cv })
        .parse(s)
}

/// Pointer declaration
#[derive(Debug, Default, PartialEq, Clone)]
pub struct Pointer<'source> {
    /// Nested name specifier (for pointer-to-member)
    path: NestedNameSpecifier<'source>,

    /// Const and volatile qualifiers,
    cv: ConstVolatile,
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

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
    fn pointer() {
        // Basic pointer syntax
        assert_eq!(super::pointer("*"), Ok(("", Pointer::default())));

        // Pointer with CV qualifier
        assert_eq!(
            super::pointer("* const"),
            Ok((
                "",
                Pointer {
                    cv: ConstVolatile::CONST,
                    ..Default::default()
                }
            ))
        );

        // Pointer to member
        assert_eq!(
            super::pointer("A::B::*"),
            Ok((
                "",
                Pointer {
                    path: vec!["A".into(), "B".into()].into(),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn pointers_reference() {
        // Empty set of qualifiers
        assert_eq!(
            super::pointers_reference(""),
            Ok(("", PointersReference::default()))
        );

        // Lone pointer qualifier
        assert_eq!(
            super::pointers_reference("*"),
            Ok((
                "",
                PointersReference {
                    pointers: vec![Pointer::default()].into(),
                    ..Default::default()
                }
            ))
        );

        // Pointer qualifier with cv annotations
        assert_eq!(
            super::pointers_reference("* const"),
            Ok((
                "",
                PointersReference {
                    pointers: vec![Pointer {
                        cv: ConstVolatile::CONST,
                        ..Default::default()
                    }]
                    .into(),
                    ..Default::default()
                }
            ))
        );

        // Multiple pointer qualifiers
        assert_eq!(
            super::pointers_reference("**"),
            Ok((
                "",
                PointersReference {
                    pointers: vec![Pointer::default(); 2].into(),
                    ..Default::default()
                }
            ))
        );

        // LValue reference
        assert_eq!(
            super::pointers_reference("&"),
            Ok((
                "",
                PointersReference {
                    reference: Reference::LValue,
                    ..Default::default()
                }
            ))
        );

        // RValue reference
        assert_eq!(
            super::pointers_reference("&&"),
            Ok((
                "",
                PointersReference {
                    reference: Reference::RValue,
                    ..Default::default()
                }
            ))
        );

        // Pointer and lvalue
        assert_eq!(
            super::pointers_reference("*const &"),
            Ok((
                "",
                PointersReference {
                    pointers: vec![Pointer {
                        cv: ConstVolatile::CONST,
                        ..Default::default()
                    }]
                    .into(),
                    reference: Reference::LValue,
                }
            ))
        );
    }
}
