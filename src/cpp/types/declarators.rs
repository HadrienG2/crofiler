//! All about declarators
//! (see https://en.cppreference.com/w/cpp/language/declarations for context)

use super::qualifiers::{self, ConstVolatile, Reference};
use crate::cpp::{
    functions::{self, FunctionSignature},
    names::{self, NestedNameSpecifier},
    values::{self, ValueLike},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser for declarators
pub fn declarator(s: &str) -> IResult<Box<[DeclOperator]>> {
    use nom::{character::complete::space0, multi::many0};
    many0(decl_operator.terminated(space0))
        .map(Vec::into_boxed_slice)
        .parse(s)
}

/// Declarator
pub type Declarator<'source> = Box<[DeclOperator<'source>]>;

/// In a type name, a declarator is composed of one or more operators
fn decl_operator(s: &str) -> IResult<DeclOperator> {
    use nom::{
        character::complete::{char, space0},
        combinator::opt,
        sequence::{delimited, separated_pair},
    };

    // Pointer declarator
    let nested_star = names::nested_name_specifier.terminated(char('*'));
    let pointer = separated_pair(nested_star, space0, qualifiers::cv)
        .map(|(path, cv)| DeclOperator::Pointer { path, cv });

    // Reference declarator
    let reference = qualifiers::reference
        .verify(|r| r != &Reference::None)
        .map(DeclOperator::Reference);

    // Array declarator
    let array = delimited(
        char('[').and(space0),
        opt(values::value_like::<false, true>),
        space0.and(char(']')),
    )
    .map(DeclOperator::Array);

    // Function declarator
    let function = functions::function_signature.map(DeclOperator::Function);

    // Parenthesized declarator (to override operator priorities
    let parenthesized = delimited(
        char('(').and(space0),
        declarator.verify(|d| d != &Declarator::default()),
        space0.and(char(')')),
    )
    .map(DeclOperator::Parenthesized);

    // Putting it all together...
    pointer
        .or(reference)
        .or(array)
        .or(function)
        .or(parenthesized)
        .parse(s)
}

/// Operators that can appear within a declarator
#[derive(Debug, PartialEq, Clone)]
pub enum DeclOperator<'source> {
    /// Pointer declarator
    Pointer {
        /// Nested name specifier (for pointer-to-member)
        path: NestedNameSpecifier<'source>,

        /// Const and volatile qualifiers,
        cv: ConstVolatile,
    },

    /// Reference declarator
    Reference(Reference),

    /// Array declarator, with optional size
    Array(Option<ValueLike<'source>>),

    /// Function declarator
    Function(FunctionSignature<'source>),

    /// Parentheses, used to override operator priorities
    Parenthesized(Declarator<'source>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn decl_operator() {
        // Basic pointer syntax
        assert_eq!(
            super::decl_operator("*"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: NestedNameSpecifier::default(),
                    cv: ConstVolatile::default(),
                }
            ))
        );

        // Pointer with CV qualifier
        assert_eq!(
            super::decl_operator("* const"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: NestedNameSpecifier::default(),
                    cv: ConstVolatile::CONST,
                }
            ))
        );

        // Pointer to member
        assert_eq!(
            super::decl_operator("A::B::*"),
            Ok((
                "",
                DeclOperator::Pointer {
                    path: vec!["A".into(), "B".into()].into(),
                    cv: ConstVolatile::default(),
                }
            ))
        );

        // Reference
        assert_eq!(
            super::decl_operator("&"),
            Ok(("", DeclOperator::Reference(Reference::LValue)))
        );

        // Array of unknown length
        assert_eq!(
            super::decl_operator("[]"),
            Ok(("", DeclOperator::Array(None)))
        );

        // Array of known length
        assert_eq!(
            super::decl_operator("[42]"),
            Ok(("", DeclOperator::Array(Some(42u8.into()))))
        );

        // Function signature
        assert_eq!(
            super::decl_operator("()"),
            Ok(("", DeclOperator::Function(FunctionSignature::default())))
        );

        // Parenthesized declarator
        assert_eq!(
            super::decl_operator("(&&)"),
            Ok((
                "",
                DeclOperator::Parenthesized(
                    vec![DeclOperator::Reference(Reference::RValue)].into()
                )
            ))
        );
    }

    #[test]
    fn declarator() {
        // Empty declarator
        assert_eq!(super::declarator(""), Ok(("", Declarator::default())));

        // Single operator
        assert_eq!(
            super::declarator("&"),
            Ok(("", vec![DeclOperator::Reference(Reference::LValue)].into()))
        );

        // Multiple operators
        assert_eq!(
            super::declarator("&&*const()"),
            Ok((
                "",
                vec![
                    DeclOperator::Reference(Reference::RValue),
                    DeclOperator::Pointer {
                        path: NestedNameSpecifier::default(),
                        cv: ConstVolatile::CONST,
                    },
                    DeclOperator::Function(FunctionSignature::default())
                ]
                .into()
            ))
        );
    }
}

// TODO: Add tests and integrate into type grammar
