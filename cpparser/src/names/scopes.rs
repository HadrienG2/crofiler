//! Handling of hierarchical scopes

use super::{
    atoms,
    unqualified::{self, UnqualifiedId},
};
use crate::{
    functions::{self, FunctionSignature},
    EntityParser, IResult,
};
use nom::Parser;
use nom_supreme::ParserExt;
use std::{fmt::Debug, path::Path};

impl EntityParser {
    /// Parser for id-expressions (= nested name-specifier + UnqualifiedId)
    pub fn parse_id_expression<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, IdExpression<'source, atoms::IdentifierKey, crate::PathKey>> {
        id_expression(
            s,
            |s| self.parse_identifier(s),
            |path| self.path_to_key(path),
        )
    }

    /// Parser for nested name-specifiers (= sequences of scopes).
    ///
    /// If you expect an UnqualifiedId after your nested-name-specifier, then what
    /// you are looking for is an id-expression, and you should use the dedicated
    /// id_expression parser for optimal performance.
    ///
    pub fn parse_nested_name_specifier<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, NestedNameSpecifier<'source, atoms::IdentifierKey, crate::PathKey>> {
        nested_name_specifier(
            s,
            |s| self.parse_identifier(s),
            |path| self.path_to_key(path),
        )
    }
}

/// Parser for id-expressions (= nested name-specifier + UnqualifiedId)
// TODO: Make private once users are migrated
pub fn id_expression<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: impl Fn(&'source str) -> PathKey,
) -> IResult<IdExpression<IdentifierKey, PathKey>> {
    use nom::combinator::map_opt;
    map_opt(
        |s| proto_id_expression(s, &parse_identifier, &path_to_key),
        |(path, id_opt)| id_opt.map(|(_backtrack, id)| IdExpression { path, id }),
    )(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdExpression<
    'source,
    IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Hierarchical scope
    path: NestedNameSpecifier<'source, IdentifierKey, PathKey>,

    /// Unqualified id-expression
    id: UnqualifiedId<'source, IdentifierKey, PathKey>,
}
//
impl<'source, T: Into<UnqualifiedId<'source, &'source str, &'source Path>>> From<T>
    for IdExpression<'source, &'source str, &'source Path>
{
    fn from(id: T) -> Self {
        Self {
            id: id.into(),
            ..Default::default()
        }
    }
}
//
impl<
        IdentifierKey: Clone + Debug + Default + PartialEq + Eq,
        PathKey: Clone + Debug + PartialEq + Eq,
    > Default for IdExpression<'_, IdentifierKey, PathKey>
{
    fn default() -> Self {
        Self {
            path: Default::default(),
            id: Default::default(),
        }
    }
}

/// Parser for nested name-specifiers (= sequences of scopes).
///
/// See EntityParser::parse_nested_name_specifier docs for semantics
///
// TODO: Make private once users are migrated
pub fn nested_name_specifier<
    'source,
    IdentifierKey: Clone + Debug + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: impl Fn(&'source str) -> PathKey,
) -> IResult<NestedNameSpecifier<IdentifierKey, PathKey>> {
    match proto_id_expression(s, parse_identifier, path_to_key) {
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
fn proto_id_expression<
    'source,
    IdentifierKey: Clone + Debug + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    mut input: &'source str,
    parse_identifier: impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: impl Fn(&'source str) -> PathKey,
) -> IResult<(
    NestedNameSpecifier<IdentifierKey, PathKey>,
    Option<(&str, UnqualifiedId<IdentifierKey, PathKey>)>,
)> {
    // Truth that the path starts at root scope (with a leading ::)
    let rooted = if let Some(rest) = input.strip_prefix("::") {
        input = rest;
        true
    } else {
        false
    };

    // Parse sequence of scope_or_unqualified_id, accumulating scopes
    let mut scopes = Vec::new();
    let make_output = |scopes: Vec<Scope<'source, IdentifierKey, PathKey>>, id_opt| {
        (
            NestedNameSpecifier {
                rooted,
                scopes: scopes.into_boxed_slice(),
            },
            id_opt,
        )
    };
    //
    while let Ok((rest, scope_or_id)) =
        scope_or_unqualified_id(input, &parse_identifier, &path_to_key)
    {
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NestedNameSpecifier<
    'source,
    IdentifierKey: Clone + Debug + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// Truth that the path starts at the root scope (leading ::)
    rooted: bool,

    /// Sequence of inner scopes
    scopes: Box<[Scope<'source, IdentifierKey, PathKey>]>,
}
//
impl<IdentifierKey: Clone + Debug + PartialEq + Eq, PathKey: Clone + Debug + PartialEq + Eq> Default
    for NestedNameSpecifier<'_, IdentifierKey, PathKey>
{
    fn default() -> Self {
        Self {
            rooted: Default::default(),
            scopes: Default::default(),
        }
    }
}

/// This parses either the Scope syntax or the UnqualifiedId syntax, in a manner
/// that avoids parsing the shared UnqualifiedId syntax twice.
fn scope_or_unqualified_id<
    'source,
    IdentifierKey: Clone + Debug + PartialEq + Eq + 'source,
    PathKey: Clone + Debug + PartialEq + Eq + 'source,
>(
    s: &'source str,
    parse_identifier: impl Fn(&'source str) -> IResult<IdentifierKey>,
    path_to_key: impl Fn(&'source str) -> PathKey,
) -> IResult<ScopeOrUnqualifiedId<IdentifierKey, PathKey>> {
    use nom::combinator::opt;
    use nom_supreme::tag::complete::tag;
    // Parse the initial UnqualifiedId
    match unqualified::unqualified_id(s, parse_identifier, path_to_key) {
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
enum ScopeOrUnqualifiedId<
    'source,
    IdentifierKey: Clone + Debug + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    Scope(Scope<'source, IdentifierKey, PathKey>),
    UnqualifiedId(UnqualifiedId<'source, IdentifierKey, PathKey>),
}
//
/// Scope (namespaces, classes, and anything else to which inner identifiers
/// could possibly belong) without recursion (that's NestedNameSpecifier).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Scope<
    'source,
    IdentifierKey: Clone + Debug + PartialEq + Eq,
    PathKey: Clone + Debug + PartialEq + Eq,
> {
    /// What identifies the scope
    id: UnqualifiedId<'source, IdentifierKey, PathKey>,

    /// When functions are scopes containing other entities (which can happen
    /// because lambdas), the function signature will be specified.
    function_signature: Option<FunctionSignature<'source>>,
}
//
impl<'source, T: Into<UnqualifiedId<'source, &'source str, &'source Path>>> From<T>
    for Scope<'source, &'source str, &'source Path>
{
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
    use crate::{names::atoms, tests::force_parse, types};
    use pretty_assertions::assert_eq;
    use std::path::Path;

    #[test]
    fn scope_or_unqualified_id() {
        let parse_scope_or_unqualified_id =
            |s| super::scope_or_unqualified_id(s, atoms::identifier, Path::new);

        // Without function signature
        assert_eq!(
            parse_scope_or_unqualified_id("std::"),
            Ok(("", ScopeOrUnqualifiedId::Scope("std".into())))
        );

        // With function signature
        assert_eq!(
            parse_scope_or_unqualified_id("my_function()::"),
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
            parse_scope_or_unqualified_id("std"),
            Ok(("", ScopeOrUnqualifiedId::UnqualifiedId("std".into())))
        );
    }

    #[test]
    fn proto_id_expression() {
        let parse_proto_id_expression =
            |s| super::proto_id_expression(s, atoms::identifier, Path::new);

        assert_eq!(parse_proto_id_expression(""), Ok(("", Default::default())));
        assert_eq!(
            parse_proto_id_expression("something"),
            Ok((
                "",
                (Default::default(), Some(("something", "something".into())))
            ))
        );
        assert_eq!(
            parse_proto_id_expression("::"),
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
            parse_proto_id_expression("::x"),
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
            parse_proto_id_expression("boost::"),
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
            parse_proto_id_expression("boost::y"),
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
            parse_proto_id_expression("::std::"),
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
            parse_proto_id_expression("::std::z 1"),
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
            parse_proto_id_expression("boost::hana::"),
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
            parse_proto_id_expression("boost::hana::stuff"),
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

    fn parse_nested_name_specifier(s: &str) -> IResult<NestedNameSpecifier<&str, &Path>> {
        super::nested_name_specifier(s, atoms::identifier, Path::new)
    }

    #[test]
    fn nested_name_specifier() {
        assert_eq!(
            parse_nested_name_specifier("boost::hana::"),
            Ok((
                "",
                NestedNameSpecifier {
                    scopes: vec!["boost".into(), "hana".into()].into(),
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parse_nested_name_specifier("boost::hana::stuff"),
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
        let parse_id_expression = |s| super::id_expression(s, atoms::identifier, Path::new);

        // Without any path
        assert_eq!(
            parse_id_expression("something"),
            Ok(("", IdExpression::from("something")))
        );

        // With a path
        assert_eq!(
            parse_id_expression("boost::hana::to_t<unsigned long long>"),
            Ok((
                "",
                IdExpression {
                    path: force_parse(parse_nested_name_specifier, "boost::hana::"),
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
