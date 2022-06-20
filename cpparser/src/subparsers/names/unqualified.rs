//! Unqualified id-expressions (those that do not feature the :: scope operator)

use crate::{
    display::{CustomDisplay, RecursionDepths},
    subparsers::{
        anonymous::{AnonymousEntity, AnonymousEntityView, Lambda, LambdaView},
        names::atoms::{IdentifierKey, IdentifierView},
        operators::{self, Operator, OperatorView},
        templates::{TemplateParameters, TemplateParametersView},
        values::{ValueKey, ValueView},
    },
    Entities, EntityParser, IResult,
};
use nom::Parser;
use std::fmt::{self, Display, Formatter};

impl EntityParser {
    /// Parser for unqualified id-expressions
    pub fn parse_unqualified_id<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, UnqualifiedId> {
        use nom::{
            character::complete::{char, space0},
            combinator::opt,
            sequence::{delimited, preceded},
        };
        use nom_supreme::tag::complete::tag;

        // Sometimes, demangler will output cv-qualified tempalte names like
        // "T const<...>". This is meaningless. const T cannot have different
        // templating behavior than T. So we ignore this syntax.
        let named_template_parameters = preceded(opt(space0.and(Self::parse_cv)), |s| {
            self.parse_template_parameters(s)
        });

        // An entity named by a user-specified identifier
        let named = |is_destructor| {
            ((|s| self.parse_identifier(s)).and(opt(named_template_parameters))).map(
                move |(id, template_parameters)| UnqualifiedId::Named {
                    is_destructor,
                    id,
                    template_parameters,
                },
            )
        };

        // An operator overload
        let operator =
            (|s| self.parse_operator_overload(s)).map(|(operator, template_parameters)| {
                UnqualifiedId::Operator {
                    operator,
                    template_parameters,
                }
            });

        // A decltype expression
        let decltype = delimited(
            tag("decltype(").and(space0),
            |s| self.parse_value_like(s, false, true),
            space0.and(char(')')),
        )
        .map(UnqualifiedId::Decltype);

        // Anonymous entities to which clang gives a name
        let mut lambda = (|s| self.parse_lambda(s)).map(UnqualifiedId::Lambda);
        let anonymous = (|s| self.parse_anonymous(s)).map(UnqualifiedId::Anonymous);

        // Operator and decltype must go before named because named matches keywords
        //
        // Since this parser is **very** hot (500M calls on a test workload), even
        // failed sub-parser trials taking ~10ns contribute to its performance, so
        // we dispatch to appropriate sub-parsers by eagerly checking the first
        // character of input. This also allows us to tell if a named entity is a
        // destructor or not. Branches other than _ are ordered by decreasing freq.
        //
        match s.as_bytes().first() {
            Some(b'{') => lambda.parse(s),
            Some(b'(') => lambda.or(anonymous).parse(s),
            Some(b'd') => decltype.or(named(false)).parse(s),
            Some(b'o') => operator.or(named(false)).parse(s),
            Some(b'~') => named(true).parse(&s[1..]),
            _ => named(false).parse(s),
        }
    }
}
//
impl Entities {
    /// Access a previously parsed unqualified id
    pub fn unqualified_id(&self, id: UnqualifiedId) -> UnqualifiedIdView {
        UnqualifiedIdView::new(id, self)
    }
}

/// Unqualified id-expression
///
/// This is the next level of complexity in C++ entity naming after raw
/// identifiers, it allows for things like templating and operator overloading
/// but not for scoping.
///
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnqualifiedId {
    /// An entity named by a user-specified identifier
    Named {
        /// Truth that this is a destructor (names starts with ~)
        is_destructor: bool,

        /// Base identifier
        id: IdentifierKey,

        /// Optional template parameters
        template_parameters: Option<TemplateParameters>,
    },

    /// An operator overload
    Operator {
        /// Which operator was overloaded
        operator: Operator,

        /// Optional template parameters
        template_parameters: Option<TemplateParameters>,
    },

    /// A decltype(<value>) expression
    Decltype(ValueKey),

    /// A lambda function, with source location information
    Lambda(Lambda),

    /// Another kind of anonymous entity from clang
    Anonymous(AnonymousEntity),
}
//
impl Default for UnqualifiedId {
    fn default() -> Self {
        Self::Anonymous(AnonymousEntity::default())
    }
}
//
impl From<IdentifierKey> for UnqualifiedId {
    fn from(id: IdentifierKey) -> Self {
        Self::Named {
            is_destructor: false,
            id,
            template_parameters: None,
        }
    }
}
//
impl From<Operator> for UnqualifiedId {
    fn from(operator: Operator) -> Self {
        Self::Operator {
            operator,
            template_parameters: None,
        }
    }
}
//
impl From<Lambda> for UnqualifiedId {
    fn from(l: Lambda) -> Self {
        Self::Lambda(l)
    }
}
//
impl From<AnonymousEntity> for UnqualifiedId {
    fn from(a: AnonymousEntity) -> Self {
        Self::Anonymous(a)
    }
}

/// View of an unqualified id-expression
#[derive(PartialEq)]
pub enum UnqualifiedIdView<'entities> {
    /// An entity named by a user-specified identifier
    Named {
        /// Truth that this is a destructor (names starts with ~)
        is_destructor: bool,

        /// Base identifier
        id: IdentifierView<'entities>,

        /// Optional template parameters
        template_parameters: Option<TemplateParametersView<'entities>>,
    },

    /// An operator overload
    Operator {
        /// Which operator was overloaded
        operator: OperatorView<'entities>,

        /// Optional template parameters
        template_parameters: Option<TemplateParametersView<'entities>>,
    },

    /// A decltype(<value>) expression
    Decltype(ValueView<'entities>),

    /// A lambda function, with source location information
    Lambda(LambdaView<'entities>),

    /// Another kind of anonymous entity from clang
    Anonymous(AnonymousEntityView<'entities>),
}
//
impl<'entities> UnqualifiedIdView<'entities> {
    /// Build an operator view
    pub fn new(id: UnqualifiedId, entities: &'entities Entities) -> Self {
        match id {
            UnqualifiedId::Named {
                is_destructor,
                id,
                template_parameters,
            } => Self::Named {
                is_destructor,
                id: entities.identifier(id),
                template_parameters: template_parameters.map(|tp| entities.template_parameters(tp)),
            },
            UnqualifiedId::Operator {
                operator,
                template_parameters,
            } => Self::Operator {
                operator: entities.operator(operator),
                template_parameters: template_parameters.map(|tp| entities.template_parameters(tp)),
            },
            UnqualifiedId::Decltype(value) => Self::Decltype(entities.value_like(value)),
            UnqualifiedId::Lambda(lambda) => Self::Lambda(entities.lambda(lambda)),
            UnqualifiedId::Anonymous(anonymous) => Self::Anonymous(entities.anonymous(anonymous)),
        }
    }
}
//
impl<'entities> Display for UnqualifiedIdView<'entities> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.display(f, RecursionDepths::ALWAYS)
    }
}
//
impl<'entities> CustomDisplay for UnqualifiedIdView<'entities> {
    fn recursion_depths(&self) -> RecursionDepths {
        match self {
            Self::Named {
                template_parameters,
                ..
            } => template_parameters.recursion_depths(),
            Self::Operator {
                operator,
                template_parameters,
            } => operator
                .recursion_depths()
                .max(template_parameters.recursion_depths()),
            // FIXME: Add decltype to list of elidable recursions
            Self::Decltype(value) => value.recursion_depths(),
            Self::Lambda(lambda) => lambda.recursion_depths(),
            Self::Anonymous(_) => RecursionDepths::NEVER,
        }
    }

    fn display(&self, f: &mut Formatter<'_>, depths: RecursionDepths) -> Result<(), fmt::Error> {
        match self {
            Self::Named {
                is_destructor,
                id,
                template_parameters,
            } => {
                if *is_destructor {
                    write!(f, "~")?;
                }
                write!(f, "{id}")?;
                template_parameters.display(f, depths)
            }
            Self::Operator {
                operator,
                template_parameters,
            } => {
                operator.display(f, depths, operators::DisplayContext::Declaration)?;
                template_parameters.display(f, depths)
            }
            Self::Decltype(value) => {
                // FIXME: Add decltype to list of elidable recursions
                write!(f, "decltype(")?;
                value.display(f, depths)?;
                write!(f, ")")
            }
            Self::Lambda(lambda) => lambda.display(f, depths),
            Self::Anonymous(anonymous) => write!(f, "{anonymous}"),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn unqualified_id() {
        let parser = EntityParser::new();

        // Just an identifier
        let identifier = |s| unwrap_parse(parser.parse_identifier(s));
        assert_eq!(
            parser.parse_unqualified_id("basic"),
            Ok(("", identifier("basic").into()))
        );

        // Destructor
        assert_eq!(
            parser.parse_unqualified_id("~stuff"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: true,
                    id: identifier("stuff"),
                    template_parameters: None,
                }
            ))
        );

        // Template with no parameters
        let template_parameters = |s| unwrap_parse(parser.parse_template_parameters(s));
        assert_eq!(
            parser.parse_unqualified_id("no_parameters<>"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: identifier("no_parameters"),
                    template_parameters: Some(template_parameters("<>")),
                }
            ))
        );

        // Template with a few parameters
        let template_parameters = |s| unwrap_parse(parser.parse_template_parameters(s));
        assert_eq!(
            parser.parse_unqualified_id("A<B, C>"),
            Ok((
                "",
                UnqualifiedId::Named {
                    is_destructor: false,
                    id: identifier("A"),
                    template_parameters: Some(template_parameters("<B, C>"))
                }
            ))
        );

        // Operator overload
        assert_eq!(
            parser.parse_unqualified_id("operator()"),
            Ok(("", Operator::CallIndex { is_index: false }.into()))
        );

        // Decltype
        assert_eq!(
            parser.parse_unqualified_id("decltype(42)"),
            Ok((
                "",
                UnqualifiedId::Decltype(unwrap_parse(parser.parse_value_like("42", true, true)))
            ))
        );

        // Lambda
        assert_eq!(
            parser.parse_unqualified_id("(lambda at /path/to/stuff.h:9876:54)"),
            Ok((
                "",
                unwrap_parse(parser.parse_lambda("(lambda at /path/to/stuff.h:9876:54)")).into()
            ))
        );

        // Anonymous entity
        assert_eq!(
            parser.parse_unqualified_id("(anonymous class)"),
            Ok((
                "",
                unwrap_parse(parser.parse_anonymous("(anonymous class)")).into()
            ))
        );
    }
}
