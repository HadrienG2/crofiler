//! Handling of hierarchical scopes

use super::unqualified::UnqualifiedId;
use crate::{
    functions::FunctionSignature, utilities::SequenceEntry, Entities, EntityParser, IResult,
};
use asylum::{lasso::Spur, sequence::SequenceKey};
use nom::Parser;
use nom_supreme::ParserExt;

/// Interned Scope sequence key
///
/// You can compare two keys as a cheaper alternative to comparing two
/// Scope sequences as long as both keys were produced by the same EntityParser.
///
/// After parsing, you can retrieve a scope sequence by passing this key
/// to the scopes() method of the Entities struct.
///
pub type ScopesKey = SequenceKey<ScopesKeyImpl, SCOPES_LEN_BITS>;
pub(crate) type ScopesKeyImpl = Spur;
pub(crate) const SCOPES_LEN_BITS: u32 = 8;
//
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
        let mut scopes = self.scopes.entry();
        let make_output = |scopes: SequenceEntry<Scope, ScopesKeyImpl, SCOPES_LEN_BITS>, id_opt| {
            (
                NestedNameSpecifier {
                    rooted,
                    scopes: scopes.intern(),
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

    /// Retrieve a scope sequence previously parsed by parse_proto_id_expression
    ///
    /// May not perform optimally, meant for validation purposes only
    ///
    #[cfg(test)]
    pub(crate) fn scope_sequence(&self, key: ScopesKey) -> Box<[Scope]> {
        self.scopes.borrow().get(key).into()
    }

    /// Total number of Scopes across all interned nested name specifiers so far
    pub fn num_scopes(&self) -> usize {
        self.scopes.borrow().num_items()
    }

    /// Maximal number of scopes in a nested name specifier
    pub fn max_scope_sequence_len(&self) -> Option<usize> {
        self.scopes.borrow().max_sequence_len()
    }

    /// This parses either the Scope syntax or the UnqualifiedId syntax, in a manner
    /// that avoids parsing the shared UnqualifiedId syntax twice.
    #[inline]
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
//
impl Entities {
    /// Retrieve a previously interned scope sequence
    pub fn scope_sequence(&self, key: ScopesKey) -> &[Scope] {
        self.scopes.get(key)
    }
}

/// C++ id-expression
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct IdExpression {
    /// Hierarchical scope
    path: NestedNameSpecifier,

    /// Unqualified id-expression
    id: UnqualifiedId,
}

/// A nested name specifier
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct NestedNameSpecifier {
    /// Truth that the path starts at the root scope (leading ::)
    rooted: bool,

    /// Sequence of inner scopes
    scopes: ScopesKey,
}
//
impl From<ScopesKey> for NestedNameSpecifier {
    fn from(scopes: ScopesKey) -> Self {
        Self {
            rooted: false,
            scopes,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::large_enum_variant)]
enum ScopeOrUnqualifiedId {
    Scope(Scope),
    UnqualifiedId(UnqualifiedId),
}

/// Scope (namespaces, classes, and anything else to which inner identifiers
/// could possibly belong) without recursion (that's NestedNameSpecifier).
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
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
                    function_signature: Some(unwrap_parse(parser.parse_function_signature("()"))),
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
        let scopes = |ss: &[&str]| -> ScopesKey {
            // Generate scope sequence
            let mut entry = parser.scopes.entry();
            let scopes: Vec<Scope> = ss
                .iter()
                .map(|s| Scope::from(unwrap_parse(parser.parse_unqualified_id(*s))))
                .collect();

            // Intern it
            for &scope in scopes.iter() {
                entry.push(scope);
            }
            let key = entry.intern();

            // Make sure it can be correctly retrieved before returning it
            for (input, result) in scopes.into_iter().zip(parser.scope_sequence(key).to_vec()) {
                assert_eq!(input, result);
            }
            key
        };

        assert_eq!(
            parser.parse_proto_id_expression(""),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: false,
                        scopes: scopes(&[]),
                    },
                    None
                )
            ))
        );
        assert_eq!(
            parser.parse_proto_id_expression("something"),
            Ok((
                "",
                (
                    NestedNameSpecifier {
                        rooted: false,
                        scopes: scopes(&[]),
                    },
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
                        scopes: scopes(&[]),
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
                        scopes: scopes(&[]),
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
                        rooted: false,
                        scopes: scopes(&["boost"]),
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
                        rooted: false,
                        scopes: scopes(&["boost"]),
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
                        scopes: scopes(&["std"])
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
                        scopes: scopes(&["std"])
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
                        rooted: false,
                        scopes: scopes(&["boost", "hana"]),
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
                        rooted: false,
                        scopes: scopes(&["boost", "hana"]),
                    },
                    Some(("stuff", unqualified_id("stuff")))
                )
            ))
        );
    }

    #[test]
    fn nested_name_specifier() {
        let parser = EntityParser::new();
        let scopes = |ss: &[&str]| -> ScopesKey {
            let mut entry = parser.scopes.entry();
            for scope in ss
                .iter()
                .map(|s| Scope::from(unwrap_parse(parser.parse_unqualified_id(*s))))
            {
                entry.push(scope);
            }
            entry.intern()
        };

        assert_eq!(
            parser.parse_nested_name_specifier("boost::hana::"),
            Ok((
                "",
                NestedNameSpecifier {
                    rooted: false,
                    scopes: scopes(&["boost", "hana"]),
                }
            ))
        );
        assert_eq!(
            parser.parse_nested_name_specifier("boost::hana::stuff"),
            Ok((
                "stuff",
                NestedNameSpecifier {
                    rooted: false,
                    scopes: scopes(&["boost", "hana"]),
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
                    path: NestedNameSpecifier {
                        rooted: false,
                        scopes: parser.scopes.entry().intern()
                    },
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
