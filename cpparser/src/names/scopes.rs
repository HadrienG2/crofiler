//! Handling of hierarchical scopes

use super::unqualified::UnqualifiedId;
use crate::{functions::FunctionSignature, EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;

impl EntityParser {
    /// Parser for id-expressions (= nested name-specifier + UnqualifiedId)
    pub fn parse_id_expression<'source>(&self, s: &'source str) -> IResult<'source, IdExpression> {
        use nom::combinator::map_opt;
        map_opt(
            |s| self.parse_proto_id_expression(s),
            |(path, id_opt)| id_opt.map(|(_backtrack, id)| IdExpression { path, id }),
        )(s)
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
    ) -> IResult<'source, NestedNameSpecifier> {
        match self.parse_proto_id_expression(s) {
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
    ///
    #[inline(always)]
    fn parse_proto_id_expression<'source>(
        &self,
        mut input: &'source str,
    ) -> IResult<'source, (NestedNameSpecifier, Option<(&'source str, UnqualifiedId)>)> {
        // Truth that the path starts at root scope (with a leading ::)
        let rooted = if let Some(rest) = input.strip_prefix("::") {
            input = rest;
            true
        } else {
            false
        };

        // Parse sequence of scope_or_unqualified_id, accumulating scopes
        let mut scopes = Vec::new();
        let make_output = |scopes: Vec<Scope>, id_opt| {
            (
                NestedNameSpecifier {
                    rooted,
                    scopes: scopes.into_boxed_slice(),
                },
                id_opt,
            )
        };
        //
        while let Ok((rest, scope_or_id)) = self.parse_scope_or_unqualified_id(input) {
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

    /// This parses either the Scope syntax or the UnqualifiedId syntax, in a manner
    /// that avoids parsing the shared UnqualifiedId syntax twice.
    fn parse_scope_or_unqualified_id<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, ScopeOrUnqualifiedId> {
        use nom::combinator::opt;
        use nom_supreme::tag::complete::tag;
        // Parse the initial UnqualifiedId
        match self.parse_unqualified_id(s) {
            // An UnqualifiedId was found, but is this actually a Scope?
            Ok((after_id, id)) => {
                match opt(|s| self.parse_function_signature(s))
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
                }
            }

            // If there is no UnqualifiedId, there is also no Scope
            Err(error) => Err(error),
        }
    }
}

/// C++ id-expression
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct IdExpression {
    /// Hierarchical scope
    path: NestedNameSpecifier,

    /// Unqualified id-expression
    id: UnqualifiedId,
}
//
impl<T: Into<UnqualifiedId>> From<T> for IdExpression {
    fn from(id: T) -> Self {
        Self {
            id: id.into(),
            ..Default::default()
        }
    }
}

/// A nested name specifier
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct NestedNameSpecifier {
    /// Truth that the path starts at the root scope (leading ::)
    rooted: bool,

    /// Sequence of inner scopes
    scopes: Box<[Scope]>,
}
//
impl<T: Into<Box<[Scope]>>> From<T> for NestedNameSpecifier {
    fn from(scopes: T) -> Self {
        Self {
            scopes: scopes.into(),
            ..Default::default()
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::large_enum_variant)]
enum ScopeOrUnqualifiedId {
    Scope(Scope),
    UnqualifiedId(UnqualifiedId),
}

/// Scope (namespaces, classes, and anything else to which inner identifiers
/// could possibly belong) without recursion (that's NestedNameSpecifier).
// FIXME: This type appears in Box<[T]>, intern that once data is owned
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Scope {
    /// What identifies the scope
    id: UnqualifiedId,

    /// When functions are scopes containing other entities (which can happen
    /// because lambdas), the function signature will be specified.
    function_signature: Option<FunctionSignature>,
}
//
impl<T: Into<UnqualifiedId>> From<T> for Scope {
    fn from(id: T) -> Self {
        Self {
            id: id.into(),
            ..Default::default()
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn scope_or_unqualified_id() {
        let parser = EntityParser::new();
        let unqualified_id = |s| unwrap_parse(parser.parse_unqualified_id(s));

        // Without function signature
        assert_eq!(
            parser.parse_scope_or_unqualified_id("std::"),
            Ok((
                "",
                ScopeOrUnqualifiedId::Scope(unqualified_id("std").into())
            ))
        );

        // With function signature
        assert_eq!(
            parser.parse_scope_or_unqualified_id("my_function()::"),
            Ok((
                "",
                ScopeOrUnqualifiedId::Scope(Scope {
                    id: unqualified_id("my_function"),
                    function_signature: Some(FunctionSignature::default()),
                })
            ))
        );

        // Without scope terminator
        assert_eq!(
            parser.parse_scope_or_unqualified_id("std"),
            Ok((
                "",
                ScopeOrUnqualifiedId::UnqualifiedId(unqualified_id("std"))
            ))
        );
    }

    #[test]
    fn proto_id_expression() {
        let parser = EntityParser::new();
        let unqualified_id = |s| unwrap_parse(parser.parse_unqualified_id(s));
        let scopes = |ss: Vec<&str>| {
            ss.into_iter()
                .map(|s| unwrap_parse(parser.parse_unqualified_id(s)).into())
                .collect()
        };

        assert_eq!(
            parser.parse_proto_id_expression(""),
            Ok(("", Default::default()))
        );
        assert_eq!(
            parser.parse_proto_id_expression("something"),
            Ok((
                "",
                (
                    Default::default(),
                    Some(("something", unqualified_id("something")))
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression("::"),
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
            parser.parse_proto_id_expression("::x"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        ..Default::default()
                    },
                    Some(("x", unqualified_id("x")))
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression("boost::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        scopes: scopes(vec!["boost"]),
                        ..Default::default()
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression("boost::y"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        scopes: scopes(vec!["boost"]),
                        ..Default::default()
                    },
                    Some(("y", unqualified_id("y")))
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression("::std::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        scopes: scopes(vec!["std"])
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression("::std::z 1"),
            Ok((
                " 1",
                (
                    NestedNameSpecifier {
                        rooted: true,
                        scopes: scopes(vec!["std"])
                    },
                    Some(("z 1", unqualified_id("z")))
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression("boost::hana::"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        scopes: scopes(vec!["boost", "hana"]),
                        ..Default::default()
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression("boost::hana::stuff"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        scopes: scopes(vec!["boost", "hana"]),
                        ..Default::default()
                    },
                    Some(("stuff", unqualified_id("stuff")))
                )
            ))
        );
    }

    #[test]
    fn nested_name_specifier() {
        let parser = EntityParser::new();
        let scopes = |ss: Vec<&str>| {
            ss.into_iter()
                .map(|s| unwrap_parse(parser.parse_unqualified_id(s)).into())
                .collect()
        };
        assert_eq!(
            parser.parse_nested_name_specifier("boost::hana::"),
            Ok((
                "",
                NestedNameSpecifier {
                    scopes: scopes(vec!["boost", "hana"]),
                    ..Default::default()
                }
            ))
        );
        assert_eq!(
            parser.parse_nested_name_specifier("boost::hana::stuff"),
            Ok((
                "stuff",
                NestedNameSpecifier {
                    scopes: scopes(vec!["boost", "hana"]),
                    ..Default::default()
                }
            ))
        );
    }

    #[test]
    fn id_expression() {
        let parser = EntityParser::new();
        let unqualified_id = |s| unwrap_parse(parser.parse_unqualified_id(s));

        // Without any path
        assert_eq!(
            parser.parse_id_expression("something"),
            Ok((
                "",
                IdExpression {
                    path: NestedNameSpecifier::default(),
                    id: unqualified_id("something")
                }
            ))
        );

        // With a path
        assert_eq!(
            parser.parse_id_expression("boost::hana::to_t<unsigned long long>"),
            Ok((
                "",
                IdExpression {
                    path: unwrap_parse(parser.parse_nested_name_specifier("boost::hana::")),
                    id: unqualified_id("to_t<unsigned long long>"),
                }
            ))
        );
    }
}
