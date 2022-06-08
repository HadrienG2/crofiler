//! Clang-provided names to C++ entities that don't have a language-defined name
//! including lambdas, anonymous classes, anonymous namespaces...

use crate::{names::atoms, EntityParser, IResult};
use nom::Parser;
use nom_supreme::ParserExt;

impl EntityParser {
    /// Parser for clang's <unknown> C++ entity, sometimes seen in ParseTemplate
    pub fn parse_unknown_entity(s: &str) -> IResult<()> {
        use nom_supreme::tag::complete::tag;
        tag("<unknown>").value(()).parse(s)
    }

    /// Parser for clang lambda types "(lambda at <file path>:<line>:<col>)"
    ///
    /// This will fail if the file path contains a ':' sign other than a
    /// Windows-style disk designator at the start, because I have no idea how
    /// to handle this inherent grammar ambiguity better...
    ///
    pub fn parse_lambda<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, Lambda<crate::PathKey>> {
        lambda(s, &|path| self.path_to_key(path))
    }

    /// Parser for other anonymous clang entities called "(anonymous <stuff>)"
    pub fn parse_anonymous<'source>(
        &self,
        s: &'source str,
    ) -> IResult<'source, AnonymousEntity<atoms::IdentifierKey>> {
        anonymous(s, &|s| self.parse_identifier(s))
    }
}

/// Parser for clang lambda types
///
/// See EntityParser::parse_lambda for general parsing semantics. `path_to_key`
/// is a function that takes a path string as input and returns a path key
/// (which may be the path itself or an interned version of it).
///
// TODO: Make private once users are migrated
pub fn lambda<'source, PathKey: 'source>(
    s: &'source str,
    path_to_key: &impl Fn(&'source str) -> PathKey,
) -> IResult<'source, Lambda<PathKey>> {
    use nom::{
        bytes::complete::{tag, take_till1},
        character::complete::{anychar, char, u32},
        combinator::{opt, recognize},
        sequence::{delimited, separated_pair},
    };

    let location = separated_pair(u32, char(':'), u32);

    let disk_designator = anychar.and(char(':'));
    let path_str = recognize(opt(disk_designator).and(take_till1(|c| c == ':')));
    let path = path_str.map(path_to_key);

    let file_location = separated_pair(path, char(':'), location);
    let lambda = file_location.map(|(file, location)| Lambda { file, location });
    delimited(tag("(lambda at "), lambda, char(')'))(s)
}
//
/// Lambda location description
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Lambda<PathKey> {
    /// In which file the lambda is declared
    file: PathKey,

    /// Where exactly in the file
    location: (Line, Column),
}
//
/// Line number within a file
pub type Line = u32;
//
/// Column number within a file
pub type Column = u32;

/// Parser for other anonymous clang entities called "(anonymous <stuff>)"
//
// TODO: Make private once users are migrated
pub fn anonymous<'source, IdentifierKey: 'source>(
    s: &'source str,
    parse_identifier: &impl Fn(&'source str) -> IResult<IdentifierKey>,
) -> IResult<'source, AnonymousEntity<IdentifierKey>> {
    use nom::{
        character::complete::char,
        combinator::opt,
        sequence::{delimited, preceded},
    };
    use nom_supreme::tag::complete::tag;
    delimited(
        tag("(anonymous"),
        opt(preceded(char(' '), parse_identifier)),
        char(')'),
    )(s)
}
//
/// Anonymous clang entity (known as "(anonymous)" or "(anonymous <something>)")
///
/// So far, only anonymous classes and namespaces were seen, but for all I know
/// there might be others... In any case, if the <something> is specified, it is
/// reported back as the string argument to this option.
pub type AnonymousEntity<IdentifierKey> = Option<IdentifierKey>;

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    #[test]
    fn unknown_entity() {
        assert_eq!(
            EntityParser::parse_unknown_entity("<unknown>"),
            Ok(("", ()))
        );
    }

    #[test]
    fn lambda() {
        let parse_lambda = |s| super::lambda(s, &Path::new);
        assert_eq!(
            parse_lambda("(lambda at /path/to/source.cpp:123:45)"),
            Ok((
                "",
                Lambda {
                    file: Path::new("/path/to/source.cpp"),
                    location: (123, 45)
                }
            ))
        );
        assert_eq!(
            parse_lambda("(lambda at c:/source.cpp:123:45)"),
            Ok((
                "",
                Lambda {
                    file: Path::new("c:/source.cpp"),
                    location: (123, 45)
                }
            ))
        );
    }

    #[test]
    fn anonymous() {
        let parse_anonymous = |s| super::anonymous(s, &atoms::identifier);
        assert_eq!(parse_anonymous("(anonymous)"), Ok(("", None)));
        assert_eq!(
            parse_anonymous("(anonymous class)"),
            Ok(("", Some("class")))
        );
        assert_eq!(
            parse_anonymous("(anonymous namespace)"),
            Ok(("", Some("namespace")))
        );
    }
}
