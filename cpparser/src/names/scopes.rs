//! Handling of hierarchical scopes

use super::unqualified::{self, UnqualifiedId};
use crate::{
    functions::{self, FunctionSignature},
    IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;

/// Parser for id-expressions (= nested name-specifier + UnqualifiedId)
pub fn id_expression(s: &str) -> IResult<IdExpression> {
    use nom::combinator::map_opt;
    map_opt(proto_id_expression, |(path, id_opt)| {
        id_opt.map(|(_backtrack, id)| IdExpression { path, id })
    })(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct IdExpression<'source> {
    /// Hierarchical scope
    path: NestedNameSpecifier<'source>,

    /// Unqualified id-expression
    id: UnqualifiedId<'source>,
}
//
impl<'source, T: Into<UnqualifiedId<'source>>> From<T> for IdExpression<'source> {
    fn from(id: T) -> Self {
        Self {
            id: id.into(),
            ..Default::default()
        }
    }
}

/// Parser for nested name-specifiers (= sequences of scopes).
///
/// If you expect an UnqualifiedId after your nested-name-specifier, then what
/// you are looking for is an id-expression, and you should use the dedicated
/// id_expression parser for optimal performance.
pub fn nested_name_specifier(s: &str) -> IResult<NestedNameSpecifier> {
    match proto_id_expression(s) {
        Ok((_rest, (path, Some((backtrack, _id))))) => Ok((backtrack, path)),
        Ok((rest, (path, None))) => Ok((rest, path)),
        Err(error) => Err(error),
    }
}

/// Parser for a nested name-specifier, optionally followed by an UnqualifiedId,
/// with an option to backtrack on that last UnqualifiedId if you don't want it.
///
/// If you make the final UnqualifiedId mandatory, you get the id_expression
/// grammar and if you backtrack on it you get the nested_name_specifier one.
#[inline(always)]
fn proto_id_expression<'source>(
    mut input: &'source str,
) -> IResult<(NestedNameSpecifier, Option<(&str, UnqualifiedId)>)> {
    // Truth that the path starts at root scope (with a leading ::)
    let rooted = if let Some(rest) = input.strip_prefix("::") {
        input = rest;
        true
    } else {
        false
    };

    // Parse sequence of scope_or_unqualified_id, accumulating scopes
    let mut scopes = Vec::new();
    let make_output = |scopes: Vec<Scope<'source>>, id_opt| {
        (
            NestedNameSpecifier {
                rooted,
                scopes: scopes.into_boxed_slice(),
            },
            id_opt,
        )
    };
    //
    while let Ok((rest, scope_or_id)) = scope_or_unqualified_id(input) {
        match scope_or_id {
            // As long as there are scopes, keep going
            ScopeOrUnqualifiedId::Scope(scope) => scopes.push(scope),

            // If a trailing UnqualifiedId is found, we reached the end of the
            // grammar, return it + input string to allow backtracking
            ScopeOrUnqualifiedId::UnqualifiedId(id) => {
                return Ok((rest, make_output(scopes, Some((input, id)))));
            }
        }

        // Keep consuming input
        input = rest;
    }

    // If control reaches this point, no trailing unqualified-id was found, the
    // scope list ended on its own
    Ok((input, make_output(scopes, None)))
}

/// A nested name specifier
#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub struct NestedNameSpecifier<'source> {
    /// Truth that the path starts at the root scope (leading ::)
    rooted: bool,

    /// Sequence of inner scopes
    scopes: Box<[Scope<'source>]>,
}

/// This parses either the Scope syntax or the UnqualifiedId syntax, in a manner
/// that avoids parsing the shared UnqualifiedId syntax twice.
fn scope_or_unqualified_id(s: &str) -> IResult<ScopeOrUnqualifiedId> {
    use nom::combinator::opt;
    use nom_supreme::tag::complete::tag;
    // Parse the initial UnqualifiedId
    match unqualified::unqualified_id(s) {
        // An UnqualifiedId was found, but is this actually a Scope?
        Ok((after_id, id)) => match opt(functions::function_signature)
            .terminated(tag("::"))
            .parse(after_id)
        {
            // Yes, return the Scope
            Ok((after_scope, function_signature)) => Ok((
                after_scope,
                ScopeOrUnqualifiedId::Scope(Scope {
                    id,
                    function_signature,
                }),
            )),

            // No, return the initial UnqualifiedId as if nothing else happened
            Err(_) => Ok((after_id, ScopeOrUnqualifiedId::UnqualifiedId(id))),
        },

        // If there is no UnqualifiedId, there is also no Scope
        Err(error) => Err(error),
    }
}
//
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::large_enum_variant)]
enum ScopeOrUnqualifiedId<'source> {
    Scope(Scope<'source>),
    UnqualifiedId(UnqualifiedId<'source>),
}
//
/// Scope (namespaces, classes, and anything else to which inner identifiers
/// could possibly belong) without recursion (that's NestedNameSpecifier).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Scope<'source> {
    /// What identifies the scope
    id: UnqualifiedId<'source>,

    /// When functions are scopes containing other entities (which can happen
    /// because lambdas), the function signature will be specified.
    function_signature: Option<FunctionSignature<'source>>,
}
//
impl<'source, T: Into<UnqualifiedId<'source>>> From<T> for Scope<'source> {
    fn from(id: T) -> Self {
        Self {
            id: id.into(),
            function_signature: None,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{tests::force_parse, types};
    use pretty_assertions::assert_eq;

    #[test]
    fn scope_or_unqualified_id() {
        // Without function signature
        assert_eq!(
            super::scope_or_unqualified_id("std::"),
            Ok(("", ScopeOrUnqualifiedId::Scope("std".into())))
        );

        // With function signature
        assert_eq!(
            super::scope_or_unqualified_id("my_function()::"),
            Ok((
                "",
                ScopeOrUnqualifiedId::Scope(Scope {
                    id: "my_function".into(),
                    function_signature: Some(FunctionSignature::default()),
                })
            ))
        );

        // Without scope terminator
        assert_eq!(
            super::scope_or_unqualified_id("std"),
            Ok(("", ScopeOrUnqualifiedId::UnqualifiedId("std".into())))
        );
    }

    #[test]
    fn proto_id_expression() {
        assert_eq!(super::proto_id_expression(""), Ok(("", Default::default())));
        assert_eq!(
            super::proto_id_expression("something"),
            Ok((
                "",
                (Default::default(), Some(("something", "something".into())))
            ))
        );
        assert_eq!(
            super::proto_id_expression("::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        ..Default::default()
                    },
                    None
                )
            ))
        );
        assert_eq!(
            super::proto_id_expression("::x"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        ..Default::default()
                    },
                    Some(("x", "x".into()))
                )
            ))
        );
        assert_eq!(
            super::proto_id_expression("boost::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        scopes: vec!["boost".into()].into(),
                        ..Default::default()
                    },
                    None
                )
            ))
        );
        assert_eq!(
            super::proto_id_expression("boost::y"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        scopes: vec!["boost".into()].into(),
                        ..Default::default()
                    },
                    Some(("y", "y".into()))
                )
            ))
        );
        assert_eq!(
            super::proto_id_expression("::std::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        scopes: vec!["std".into()].into()
                    },
                    None
                )
            ))
        );
        assert_eq!(
            super::proto_id_expression("::std::z 1"),
            Ok((
                " 1",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        scopes: vec!["std".into()].into()
                    },
                    Some(("z 1", "z".into()))
                )
            ))
        );
        assert_eq!(
            super::proto_id_expression("boost::hana::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        scopes: vec!["boost".into(), "hana".into()].into(),
                        ..Default::default()
                    },
                    None
                )
            ))
        );
        assert_eq!(
            super::proto_id_expression("boost::hana::stuff"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        scopes: vec!["boost".into(), "hana".into()].into(),
                        ..Default::default()
                    },
                    Some(("stuff", "stuff".into()))
                )
            ))
        );
    }

    #[test]
    fn nested_name_specifier() {
        assert_eq!(
            super::nested_name_specifier("boost::hana::"),
            Ok((
                "",
                NestedNameSpecifier {
                    scopes: vec!["boost".into(), "hana".into()].into(),
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            super::nested_name_specifier("boost::hana::stuff"),
            Ok((
                "stuff",
                NestedNameSpecifier {
                    scopes: vec!["boost".into(), "hana".into()].into(),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn id_expression() {
        // Without any path
        assert_eq!(
            super::id_expression("something"),
            Ok(("", IdExpression::from("something")))
        );

        // With a path
        assert_eq!(
            super::id_expression("boost::hana::to_t<unsigned long long>"),
            Ok((
                "",
                IdExpression {
                    path: force_parse(super::nested_name_specifier, "boost::hana::"),
                    id: UnqualifiedId::Named {
                        is_destructor: false,
                        id: "to_t",
                        template_parameters: Some(Some(
                            vec![force_parse(types::type_like, "unsigned long long").into()].into()
                        ))
                    }
                }
            ))
        );
    }
}
