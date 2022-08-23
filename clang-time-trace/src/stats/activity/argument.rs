//! Facilities for handling clang activity arguments

use crate::{ClangTrace, InternedPath, PathError, PathKey};
use cpp_demangle::{DemangleOptions, ParseOptions, Symbol};
use cpparser::{nom, EntityKey, EntityParser, EntityView};
use log::trace;
use std::{cell::RefCell, rc::Rc};
use thiserror::Error;

/// Tuned 10x above maximum observed requirement
const CPP_DEMANGLE_RECURSION_LIMIT: u32 = 1024;

/// Activity argument that underwent basic validation, but not yet full parsing
//
// Due to how the Rust borrow checker works, one needs to clone this data in
// order to parse it, so it has been made cheap to clone.
//
// At the JSON data level, this type represents the contents of the "detail"
// field, along with instructions on how it should be parsed. Prior validation
// has already assessed that if a detail field should be present, it is present,
// so the Option can be unwrapped with confidence.
//
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RawActivityArgument {
    /// How this activity argument should be parsed
    arg_type: ActivityArgumentType,

    /// What the "detail" string of the JSON source contained
    detail: Option<Rc<str>>,
}
//
impl RawActivityArgument {
    /// Record an activity argument, knowing its intended parsing logic and the
    /// "detail" string payload that was provided in the JSON file.
    pub(crate) fn new(arg_type: ActivityArgumentType, detail: Option<Rc<str>>) -> Self {
        Self { arg_type, detail }
    }

    /// Check how this activity argument is supposed to be parsed
    pub fn arg_type(&self) -> ActivityArgumentType {
        self.arg_type
    }

    /// Get a copy of the raw "detail" string from JSON
    pub fn detail(&self) -> Option<Rc<str>> {
        self.detail.clone()
    }

    /// Parse the activity argument
    pub fn parse(
        self,
        trace: &mut ClangTrace,
    ) -> Result<ParsedActivityArgument, ActivityArgumentError> {
        let (parser, demangling_buf) = trace.parser_and_demangling_buf();
        self.parse_impl(parser, demangling_buf)
    }

    /// Lower-level version of parse() used in unit tests
    fn parse_impl(
        self,
        parser: &mut EntityParser,
        demangling_buf: &mut String,
    ) -> Result<ParsedActivityArgument, ActivityArgumentError> {
        let detail = RefCell::new(self.detail);
        let has_detail = || detail.borrow().is_some();
        let detail_opt = || detail.borrow_mut().take();
        let detail = || detail_opt().expect("Presence should be checked upstream");
        //
        let mut mangled_arg =
            |parser: &mut EntityParser| -> Result<ParsedMangledSymbol, ActivityArgumentError> {
                Self::parse_mangled_symbol(detail(), parser, demangling_buf)
            };
        //
        let parse_unnamed_loop_arg =
            || -> Result<(), ActivityArgumentError> { Self::parse_unnamed_loop(detail()) };
        //
        match self.arg_type {
            ActivityArgumentType::Nothing => {
                debug_assert_eq!(detail_opt(), None);
                Ok(ParsedActivityArgument::Nothing)
            }

            ActivityArgumentType::String => Ok(ParsedActivityArgument::String(detail())),

            ActivityArgumentType::FilePath => Ok(ParsedActivityArgument::FilePath(
                parser.intern_path(&detail()),
            )),

            ActivityArgumentType::CppEntity => {
                Self::parse_entity(&detail(), parser).map(ParsedActivityArgument::CppEntity)
            }

            ActivityArgumentType::MangledSymbol => {
                mangled_arg(parser).map(ParsedActivityArgument::MangledSymbol)
            }

            ActivityArgumentType::MangledSymbolOpt => {
                if has_detail() {
                    mangled_arg(parser).map(ParsedActivityArgument::MangledSymbol)
                } else {
                    Ok(ParsedActivityArgument::Nothing)
                }
            }

            ActivityArgumentType::UnnamedLoop => {
                parse_unnamed_loop_arg().map(|()| ParsedActivityArgument::UnnamedLoop)
            }

            ActivityArgumentType::UnnamedLoopOpt => {
                if has_detail() {
                    parse_unnamed_loop_arg().map(|()| ParsedActivityArgument::UnnamedLoop)
                } else {
                    Ok(ParsedActivityArgument::Nothing)
                }
            }
        }
    }

    /// Parse a "detail" argument payload that contains a C++ entity name
    fn parse_entity(
        s: &str,
        parser: &mut EntityParser,
    ) -> Result<EntityKey, ActivityArgumentError> {
        parser.parse_entity(s).map_err(|e| {
            ActivityArgumentError::from(nom::error::Error::new(Box::<str>::from(e.input), e.code))
        })
    }

    /// Parse a "detail" argument payload that contains a mangled C++ symbol
    fn parse_mangled_symbol(
        symbol: Rc<str>,
        parser: &mut EntityParser,
        demangling_buf: &mut String,
    ) -> Result<ParsedMangledSymbol, ActivityArgumentError> {
        let mut parse_demangled = |entity: Rc<str>| -> ParsedMangledSymbol {
            if let Ok(parsed) = Self::parse_entity(&*entity, parser) {
                ParsedMangledSymbol::Parsed(parsed)
            } else {
                ParsedMangledSymbol::Demangled(entity)
            }
        };

        let demangling_result = match Symbol::new_with_options(
            &*symbol,
            &ParseOptions::default().recursion_limit(CPP_DEMANGLE_RECURSION_LIMIT),
        )
        .map(|s| {
            demangling_buf.clear();
            s.structured_demangle(
                &mut *demangling_buf,
                &DemangleOptions::default()
                    .hide_expression_literal_types()
                    .no_return_type()
                    .recursion_limit(CPP_DEMANGLE_RECURSION_LIMIT),
            )
        }) {
            // Mangled symbol was successfully demangled, intern it along with the rest
            Ok(Ok(())) => parse_demangled(demangling_buf.clone().into()),

            // Symbol failed to demangle, try some patterns that cpp_demangle
            // should not reject but actually does reject before giving up
            Ok(Err(_)) | Err(_) => match &*symbol {
                "main" | "__clang_call_terminate" => parse_demangled(symbol),
                _ => ParsedMangledSymbol::Mangled(symbol),
            },
        };

        match &demangling_result {
            ParsedMangledSymbol::Parsed(_) => {}
            ParsedMangledSymbol::Demangled(d) => trace!("Failed to parse demangled symbol {d:?}"),
            ParsedMangledSymbol::Mangled(m) => trace!("Failed to demangle symbol {m:?}"),
        }

        Ok(demangling_result)
    }

    /// Handling of the "<unnamed loop>" constant argument
    fn parse_unnamed_loop(loop_name: Rc<str>) -> Result<(), ActivityArgumentError> {
        if &*loop_name == "<unnamed loop>" {
            Ok(())
        } else {
            let loop_name: &str = &loop_name;
            Err(ActivityArgumentError::UnexpectedLoopName(loop_name.into()))
        }
    }
}

/// Empirically observed activity argument parsing logics for time-trace entries
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ActivityArgumentType {
    /// No argument
    Nothing,

    /// An arbitrary string
    String,

    /// An interned file path
    FilePath,

    /// A C++ entity (class, function, ...)
    CppEntity,

    /// A C++ mangled symbol
    MangledSymbol,

    /// Either a C++ mangled symbol or nothing
    MangledSymbolOpt,

    /// The "<unnamed loop>" constant string (loops can't be named in C++)
    UnnamedLoop,

    /// Either the "<unnamed loop>" constant string or nothing
    UnnamedLoopOpt,
}

/// Stored data about an activity's argument
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParsedActivityArgument {
    /// No argument
    Nothing,

    /// An arbitrary string
    String(Rc<str>),

    /// An interned file path
    FilePath(PathKey),

    /// A C++ entity (class, function, ...)
    CppEntity(EntityKey),

    /// A C++ mangled symbol
    MangledSymbol(ParsedMangledSymbol),

    /// The "<unnamed loop>" constant string (loops can't be named in C++)
    UnnamedLoop,
}
//
impl ParsedActivityArgument {
    /// Resolve associated ActivityArgumentView
    pub fn resolve<'a>(&'a self, trace: &'a ClangTrace) -> ActivityArgument<'a> {
        match self {
            ParsedActivityArgument::Nothing => ActivityArgument::Nothing,
            ParsedActivityArgument::String(s) => ActivityArgument::String(&*s),
            ParsedActivityArgument::FilePath(p) => ActivityArgument::FilePath(trace.file_path(*p)),
            ParsedActivityArgument::CppEntity(e) => ActivityArgument::CppEntity(trace.entity(*e)),
            ParsedActivityArgument::MangledSymbol(m) => {
                ActivityArgument::MangledSymbol(m.resolve(trace))
            }
            ParsedActivityArgument::UnnamedLoop => ActivityArgument::UnnamedLoop,
        }
    }
}
//
/// Concrete things that an activity can take as an argument
#[derive(PartialEq)]
pub enum ActivityArgument<'trace> {
    /// No argument
    Nothing,

    /// An arbitrary string
    String(&'trace str),

    /// An interned file path
    FilePath(InternedPath<'trace>),

    /// A C++ entity (class, function, ...)
    CppEntity(EntityView<'trace>),

    /// A C++ mangled symbol
    MangledSymbol(MangledSymbol<'trace>),

    /// The "<unnamed loop>" constant string (loops can't be named in C++)
    UnnamedLoop,
}

/// Stored data about a mangled C++ symbol that we tried to demangle and parse
///
/// See MangledSymbol for more documentation
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParsedMangledSymbol {
    /// Symbol was successfully mangled and interned
    Parsed(EntityKey),

    /// The symbol was demangled, but could not be parsed into an AST
    Demangled(Rc<str>),

    /// Demangling failed and the symbol was kept in its original form.
    Mangled(Rc<str>),
}
//
impl ParsedMangledSymbol {
    /// Resolve the full mangled symbol data
    pub fn resolve<'a>(&'a self, trace: &'a ClangTrace) -> MangledSymbol<'a> {
        match self {
            ParsedMangledSymbol::Parsed(key) => MangledSymbol::Parsed(trace.entity(*key)),
            ParsedMangledSymbol::Demangled(s) => MangledSymbol::Demangled(&*s),
            ParsedMangledSymbol::Mangled(s) => MangledSymbol::Mangled(&*s),
        }
    }
}
//
/// A mangled C++ symbol that we tried to demangle and parse
#[derive(PartialEq)]
pub enum MangledSymbol<'trace> {
    /// Symbol was successfully mangled and interned
    Parsed(EntityView<'trace>),

    /// The symbol was demangled, but could not be parsed into an AST
    ///
    /// This normally happens when the demangler emits ill-formed output such
    /// as `SomeTemplate<int, && >` or `()...`. If you find reasonable output
    /// which we do not parse, please submit it as a bug.
    ///
    Demangled(&'trace str),

    /// Demangling failed and the symbol was kept in its original form.
    ///
    /// Typical patterns that fail to demangle include
    /// - __cxx_global_var_init(.<number>)?
    /// - _GLOBAL__sub_I_<source file>
    ///
    Mangled(&'trace str),
}

/// What can go wrong while parsing an Activity's argument
#[derive(Error, Debug, PartialEq)]
pub enum ActivityArgumentError {
    /// Encountered an unexpected activity file path
    #[error("failed to parse activity file path ({0})")]
    BadFilePath(#[from] PathError),

    /// Failed to parse a C++ entity name
    #[error("failed to parse C++ entity ({0})")]
    BadCppEntity(#[from] nom::error::Error<Box<str>>),

    /// Unexpected loop name
    #[error("unexpected loop name ({0})")]
    UnexpectedLoopName(Box<str>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_nothing() {
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::Nothing,
                detail: None
            }
            .parse_impl(&mut EntityParser::new(), &mut String::new()),
            Ok(ParsedActivityArgument::Nothing)
        );
    }

    #[test]
    fn parse_string() {
        const MOCK_STR: &str = "X86 DAG->DAG Instruction Selection";
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::String,
                detail: Some(MOCK_STR.into())
            }
            .parse_impl(&mut EntityParser::new(), &mut String::new()),
            Ok(ParsedActivityArgument::String(MOCK_STR.into()))
        );
    }

    #[test]
    fn parse_path() {
        const MOCK_PATH: &str =
            "/mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp";
        let mut parser = EntityParser::new();
        let path_key = parser.intern_path(MOCK_PATH);
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::FilePath,
                detail: Some(MOCK_PATH.into())
            }
            .parse_impl(&mut parser, &mut String::new()),
            Ok(ParsedActivityArgument::FilePath(path_key))
        );
    }

    #[test]
    fn parse_entity() {
        const MOCK_ENTITY: &str = "Acts::Test::MeasurementCreator";
        let mut parser = EntityParser::new();
        let entity_key = parser
            .parse_entity(MOCK_ENTITY)
            .expect("Known-good parse, shouldn't fail");
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::CppEntity,
                detail: Some(MOCK_ENTITY.into())
            }
            .parse_impl(&mut parser, &mut String::new()),
            Ok(ParsedActivityArgument::CppEntity(entity_key))
        );
    }

    #[test]
    fn parse_symbol() {
        // Commonalities betwwen mangled symbol tests
        let test_mangled_symbol =
            |entity_parser: &mut EntityParser,
             symbol: &str,
             expected_parse: ParsedMangledSymbol| {
                for arg_type in [
                    ActivityArgumentType::MangledSymbol,
                    ActivityArgumentType::MangledSymbolOpt,
                ] {
                    assert_eq!(
                        RawActivityArgument {
                            arg_type,
                            detail: Some(symbol.into())
                        }
                        .parse_impl(&mut *entity_parser, &mut String::new()),
                        Ok(ParsedActivityArgument::MangledSymbol(
                            expected_parse.clone()
                        ))
                    );
                }
            };
        let mut parser = EntityParser::new();

        // Mangled symbol that demangles
        const VALID: &'static str = "_ZN4Acts4Test29comb_kalman_filter_zero_field11test_methodEv";
        let key = parser
            .parse_entity("Acts::Test::comb_kalman_filter_zero_field::test_method()")
            .expect("Known-good parse, shouldn't fail");
        test_mangled_symbol(&mut parser, VALID, ParsedMangledSymbol::Parsed(key));

        // Mangled symbol that doesn't demangle
        const INVALID: &'static str = "__cxx_global_var_init.1";
        test_mangled_symbol(
            &mut parser,
            INVALID,
            ParsedMangledSymbol::Mangled(INVALID.into()),
        );

        // Optional mangled symbol that isn't present
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::MangledSymbolOpt,
                detail: None
            }
            .parse_impl(&mut parser, &mut String::new()),
            Ok(ParsedActivityArgument::Nothing)
        );
    }

    #[test]
    fn parse_unnamed_loop() {
        // Unnamed loop argument
        for arg_type in [
            ActivityArgumentType::UnnamedLoop,
            ActivityArgumentType::UnnamedLoopOpt,
        ] {
            assert_eq!(
                RawActivityArgument {
                    arg_type,
                    detail: Some("<unnamed loop>".into())
                }
                .parse_impl(&mut EntityParser::new(), &mut String::new()),
                Ok(ParsedActivityArgument::UnnamedLoop)
            );
        }

        // Optional unnamed loop argument that isn't present
        assert_eq!(
            RawActivityArgument {
                arg_type: ActivityArgumentType::UnnamedLoopOpt,
                detail: None
            }
            .parse_impl(&mut EntityParser::new(), &mut String::new()),
            Ok(ParsedActivityArgument::Nothing)
        );
    }
}
