//! Id-expression related parsing
//!
//! In the C++ grammar, id-expressions are things which are more complex than
//! identifiers but can be used in place of identifiers, such as qualified
//! identifiers (path::to::something) or templated names (A<B>).

use crate::cpp::{
    anonymous::{self, AnonymousEntity, Lambda},
    atoms,
    functions::{self, FunctionSignature},
    operators::{self, Operator},
    templates::{self, TemplateParameters},
    values::{self, ValueLike},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser for id-expressions
pub fn id_expression(s: &str) -> IResult<IdExpression> {
    (nested_name_specifier.and(unqualified_id))
        .map(|(path, id)| IdExpression { path, id })
        .parse(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, Default, PartialEq)]
pub struct IdExpression<'source> {
    /// Hierarchical scope
    path: NestedNameSpecifier<'source>,

    /// Unqualified id-expression
    id: UnqualifiedId<'source>,
}
//
impl<'source> From<UnqualifiedId<'source>> for IdExpression<'source> {
    fn from(id: UnqualifiedId<'source>) -> Self {
        Self {
            path: Default::default(),
            id,
        }
    }
}
//
impl<'source> From<&'source str> for IdExpression<'source> {
    fn from(id: &'source str) -> Self {
        Self {
            path: Default::default(),
            id: UnqualifiedId::from(id),
        }
    }
}

/// Parser for nested name-specifiers (= a sequence of scopes)
pub fn nested_name_specifier(s: &str) -> IResult<NestedNameSpecifier> {
    use nom::multi::many0;
    many0(scope).map(Vec::into_boxed_slice).parse(s)
}

/// A nested name specifier
pub type NestedNameSpecifier<'source> = Box<[Scope<'source>]>;

/// Parser for unqualified id-expressions
fn unqualified_id(s: &str) -> IResult<UnqualifiedId> {
    use nom::{
        character::complete::{char, space0},
        combinator::opt,
        sequence::{delimited, tuple},
    };
    use nom_supreme::tag::complete::tag;

    // An entity named by a user-specified identifier
    let is_destructor = opt(char('~')).map(|opt| opt.is_some());
    let named = tuple((
        is_destructor,
        atoms::identifier,
        opt(templates::template_parameters),
    ))
    .map(
        |(is_destructor, id, template_parameters)| UnqualifiedId::Named {
            is_destructor,
            id,
            template_parameters,
        },
    );

    // An operator overload
    let operator = operators::operator_overload.map(|(operator, template_parameters)| {
        UnqualifiedId::Operator {
            operator,
            template_parameters,
        }
    });

    // A decltype expression
    let decltype = delimited(
        tag("decltype(").and(space0),
        values::value_like::<false, true>,
        space0.and(char(')')),
    )
    .map(Box::new)
    .map(UnqualifiedId::Decltype);

    // Anonymous entities to which clang gives a name
    let lambda = anonymous::lambda.map(UnqualifiedId::Lambda);
    let anonymous = anonymous::anonymous.map(UnqualifiedId::Anonymous);

    // Operator and decltype must go before named because named matches keywords
    operator
        .or(decltype)
        .or(named)
        .or(lambda)
        .or(anonymous)
        .parse(s)
}
//
#[derive(Clone, Debug, PartialEq)]
pub enum UnqualifiedId<'source> {
    /// An entity named by a user-specified identifier
    Named {
        /// Truth that this is a destructor (names starts with ~)
        is_destructor: bool,

        /// Base identifier
        id: &'source str,

        /// Optional template parameters
        template_parameters: Option<TemplateParameters<'source>>,
    },

    /// An operator overload
    Operator {
        /// Which operator was overloaded
        operator: Operator<'source>,

        /// Optional template parameters
        template_parameters: Option<TemplateParameters<'source>>,
    },

    /// A decltype(<value>) expression
    Decltype(Box<ValueLike<'source>>),

    /// A lambda function, with source location information
    Lambda(Lambda<'source>),

    /// Another kind of anonymous entity from clang
    Anonymous(AnonymousEntity<'source>),
}
//
impl Default for UnqualifiedId<'_> {
    fn default() -> Self {
        Self::Anonymous(AnonymousEntity::default())
    }
}
//
impl<'source> From<&'source str> for UnqualifiedId<'source> {
    fn from(id: &'source str) -> Self {
        Self::Named {
            is_destructor: false,
            id,
            template_parameters: None,
        }
    }
}

/// Parser for scopes (things that contain other things)
fn scope(s: &str) -> IResult<Scope> {
    use nom::combinator::opt;
    use nom_supreme::tag::complete::tag;
    (unqualified_id
        .and(opt(functions::function_signature))
        .terminated(tag("::")))
    .map(|(id, function_signature)| Scope {
        id,
        function_signature,
    })
    .parse(s)
}
//
/// Scope (namespaces, classes, and anything else to which inner identifiers
/// could possibly belong).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Scope<'source> {
    /// What identifies the scope
    id: UnqualifiedId<'source>,

    /// When functions are scopes containing other entities (which can happen
    /// because lambdas), the function signature will be specified.
    function_signature: Option<FunctionSignature<'source>>,
}
//
impl<'source> From<&'source str> for Scope<'source> {
    fn from(id: &'source str) -> Self {
        Self {
            id: id.into(),
            function_signature: None,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::cpp::tests::force_parse_type;
    use pretty_assertions::assert_eq;

    #[test]
    fn unqualified_id() {
        // Just an identifier
        assert_eq!(
            super::unqualified_id("basic"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: "basic",
                    template_parameters: None,
                }
            ))
        );

        // Destructor
        assert_eq!(
            super::unqualified_id("~stuff"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: true,
                    id: "stuff",
                    template_parameters: None,
                }
            ))
        );

        // Template with no parameters
        assert_eq!(
            super::unqualified_id("no_parameters<>"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: "no_parameters",
                    template_parameters: Some(Some(vec![].into())),
                }
            ))
        );

        // Template with a few parameters
        assert_eq!(
            super::unqualified_id("A<B, C>"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: "A",
                    template_parameters: Some(Some(
                        vec![force_parse_type("B").into(), force_parse_type("C").into()].into()
                    ))
                }
            ))
        );

        // Operator overload
        assert_eq!(
            super::unqualified_id("operator()"),
            Ok((
                "",
                UnqualifiedId::Operator {
                    operator: Operator::CallIndex { is_index: false },
                    template_parameters: Default::default(),
                }
            ))
        );

        // Decltype
        assert_eq!(
            super::unqualified_id("decltype(42)"),
            Ok(("", UnqualifiedId::Decltype(Box::new(42u8.into()))))
        );

        // Lambda
        assert_eq!(
            super::unqualified_id("(lambda at /path/to/stuff.h:9876:54)"),
            Ok((
                "",
                UnqualifiedId::Lambda(
                    anonymous::lambda("(lambda at /path/to/stuff.h:9876:54)")
                        .unwrap()
                        .1
                )
            ))
        );

        // Anonymous entity
        assert_eq!(
            super::unqualified_id("(anonymous class)"),
            Ok((
                "",
                UnqualifiedId::Anonymous(anonymous::anonymous("(anonymous class)").unwrap().1)
            ))
        );
    }

    #[test]
    fn scope() {
        // Without function signature
        assert_eq!(
            super::scope("std::"),
            Ok((
                "",
                Scope {
                    id: "std".into(),
                    function_signature: None,
                }
            ))
        );

        // With function signature
        assert_eq!(
            super::scope("my_function()::"),
            Ok((
                "",
                Scope {
                    id: "my_function".into(),
                    function_signature: Some(FunctionSignature::default()),
                }
            ))
        );
    }

    #[test]
    fn nested_name_specifier() {
        assert_eq!(
            super::nested_name_specifier(""),
            Ok(("", Default::default()))
        );
        assert_eq!(
            super::nested_name_specifier("boost::"),
            Ok(("", vec!["boost".into()].into()))
        );
        assert_eq!(
            super::nested_name_specifier("boost::hana::"),
            Ok(("", vec!["boost".into(), "hana".into()].into()))
        );
    }

    #[test]
    fn id_expression() {
        // Without any path
        assert_eq!(
            super::id_expression("something"),
            Ok((
                "",
                IdExpression {
                    path: vec![].into(),
                    id: "something".into(),
                }
            ))
        );

        // With a path
        assert_eq!(
            super::id_expression("boost::hana::to_t<unsigned long long>"),
            Ok((
                "",
                IdExpression {
                    path: vec!["boost".into(), "hana".into()].into(),
                    id: UnqualifiedId::Named {
                        is_destructor: false,
                        id: "to_t",
                        template_parameters: Some(Some(
                            vec![force_parse_type("unsigned long long").into()].into()
                        ))
                    }
                }
            ))
        );
    }
}
