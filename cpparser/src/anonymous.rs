//! Clang-provided names to C++ entities that don't have a language-defined name
//! including lambdas, anonymous classes, anonymous namespaces...

use crate::{names::atoms::IdentifierKey, EntityParser, IResult, PathKey};
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
    pub fn parse_lambda<'source>(&self, s: &'source str) -> IResult<'source, Lambda> {
        use nom::{
            bytes::complete::{tag, take_till1},
            character::complete::{anychar, char, u32},
            combinator::{opt, recognize},
            sequence::{delimited, separated_pair},
        };

        let location = separated_pair(u32, char(':'), u32);

        let disk_designator = anychar.and(char(':'));
        let path_str = recognize(opt(disk_designator).and(take_till1(|c| c == ':')));
        let path = path_str.map(|path| self.path_to_key(path));

        let file_location = separated_pair(path, char(':'), location);
        let lambda = file_location.map(|(file, location)| Lambda { file, location });
        delimited(tag("(lambda at "), lambda, char(')'))(s)
    }

    /// Parser for other anonymous clang entities called "(anonymous <stuff>)"
    pub fn parse_anonymous<'source>(&self, s: &'source str) -> IResult<'source, AnonymousEntity> {
        use nom::{
            character::complete::char,
            combinator::opt,
            sequence::{delimited, preceded},
        };
        use nom_supreme::tag::complete::tag;
        delimited(
            tag("(anonymous"),
            opt(preceded(char(' '), |s| self.parse_identifier(s))),
            char(')'),
        )(s)
    }
}

/// Lambda location description
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Lambda {
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

/// Anonymous clang entity (known as "(anonymous)" or "(anonymous <something>)")
///
/// So far, only anonymous classes and namespaces were seen, but for all I know
/// there might be others... In any case, if the <something> is specified, it is
/// reported back as the string argument to this option.
pub type AnonymousEntity = Option<IdentifierKey>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::unwrap_parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn unknown_entity() {
        assert_eq!(
            EntityParser::parse_unknown_entity("<unknown>"),
            Ok(("", ()))
        );
    }

    #[test]
    fn lambda() {
        let parser = EntityParser::new();
        if cfg!(target_os = "windows") {
            assert_eq!(
                parser.parse_lambda("(lambda at c:/source.cpp:123:45)"),
                Ok((
                    "",
                    Lambda {
                        file: parser.path_to_key("c:/source.cpp"),
                        location: (123, 45)
                    }
                ))
            );
        } else {
            assert_eq!(
                parser.parse_lambda("(lambda at /path/to/source.cpp:123:45)"),
                Ok((
                    "",
                    Lambda {
                        file: parser.path_to_key("/path/to/source.cpp"),
                        location: (123, 45)
                    }
                ))
            );
        }
    }

    #[test]
    fn anonymous() {
        let parser = EntityParser::new();
        let identifier = |s| unwrap_parse(parser.parse_identifier(s));
        assert_eq!(parser.parse_anonymous("(anonymous)"), Ok(("", None)));
        assert_eq!(
            parser.parse_anonymous("(anonymous class)"),
            Ok(("", Some(identifier("class"))))
        );
        assert_eq!(
            parser.parse_anonymous("(anonymous namespace)"),
            Ok(("", Some(identifier("namespace"))))
        );
    }
}
