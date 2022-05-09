//! Easier C++ build profiling

#![deny(missing_docs)]

mod path;

use clang_time_trace::{ActivityArgument, ClangTrace, Duration};
use nom::IResult;
use std::{collections::HashMap, path::Path};
use unicode_xid::UnicodeXID;

fn main() {
    let trace =
        ClangTrace::from_file("2020-05-25_CombinatorialKalmanFilterTests.cpp.json").unwrap();

    println!("Profile from {}", trace.process_name());

    // Total clang execution time
    let root_duration = trace
        .root_activities()
        .map(|root| root.duration())
        .sum::<f64>();

    // Activity types by self-duration
    println!("\nSelf-duration breakdown by activity type:");
    //
    let mut profile = HashMap::<_, Duration>::new();
    for activity_trace in trace.all_activities() {
        *profile.entry(activity_trace.activity().name()).or_default() +=
            activity_trace.self_duration();
    }
    //
    let mut profile = profile.into_iter().collect::<Box<[_]>>();
    profile.sort_unstable_by(|(_, d1), (_, d2)| d2.partial_cmp(d1).unwrap());
    //
    for (name, duration) in profile.iter() {
        let percent = duration / root_duration * 100.0;
        println!("- {name} ({duration} µs, {percent:.2} %)");
    }

    // Flat activity profile by self-duration
    const FLAT_PROFILE_THRESHOLD: Duration = 0.01;
    println!("\nHot activities by self-duration:");
    //
    let norm = 1.0 / root_duration;
    let mut activities = trace
        .all_activities()
        .filter(|a| a.self_duration() * norm >= FLAT_PROFILE_THRESHOLD)
        .collect::<Box<[_]>>();
    //
    activities
        .sort_unstable_by(|a1, a2| a2.self_duration().partial_cmp(&a1.self_duration()).unwrap());
    //
    for activity_trace in activities.iter() {
        let activity = activity_trace.activity();
        let self_duration = activity_trace.self_duration();
        let percent = self_duration * norm * 100.0;
        println!("- {activity:?} ({self_duration} µs, {percent:.2} %)");
    }
    //
    let num_activities = trace.all_activities().count();
    if activities.len() < num_activities {
        let other_activities = num_activities - activities.len();
        println!(
            "- ... and {other_activities} other activities below {} % threshold ...",
            FLAT_PROFILE_THRESHOLD * 100.0
        );
    }

    // Print a list of file paths
    println!("\nFile paths:");
    let (width, _height) = termion::terminal_size().unwrap();
    for activity_trace in trace.all_activities() {
        if let ActivityArgument::FilePath(p) = activity_trace.activity().argument() {
            println!(
                "- {}",
                path::truncate_path(&trace.file_path(&p), width.min(80))
            )
        }
    }

    // Print a list of C++ entities that the parser doesn't handle yet
    println!("\nExamples of incompletely or wrongly parsed C++ entities:");
    let mut displayed = 0;
    const MAX_DISPLAY: usize = 30;
    for activity_trace in trace.all_activities() {
        if let ActivityArgument::CppEntity(e) = activity_trace.activity().argument() {
            match entity(&e) {
                Ok(("", _)) => {}
                other => {
                    println!("- {other:?}");
                    displayed += 1;
                    if displayed == MAX_DISPLAY {
                        break;
                    }
                }
            }
        }
    }

    // Hierarchical profile prototype
    // (TODO: Make this more hierarchical and display using termtree)
    println!("\nTree roots:");
    for root in trace.root_activities() {
        println!("- {root:#?}");
    }
}

/// Parser for C++ identifiers
fn identifier(s: &str) -> IResult<&str, &str> {
    use nom::{
        character::complete::satisfy, combinator::recognize, multi::many0_count, sequence::pair,
    };
    recognize(pair(
        satisfy(|c| c.is_xid_start() || c == '_'),
        many0_count(satisfy(UnicodeXID::is_xid_continue)),
    ))(s)
}

/// Parser for C++ integer literals
use nom::character::complete::i128 as integer_literal;

/// Parser for CV qualifiers
fn cv(s: &str) -> IResult<&str, ConstVolatile> {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::space1,
        combinator::{map, opt},
        sequence::{pair, preceded},
    };
    let const_ = || {
        map(tag("const"), |_| ConstVolatile {
            is_const: true,
            is_volatile: false,
        })
    };
    let volatile = || {
        map(tag("volatile"), |_| ConstVolatile {
            is_const: false,
            is_volatile: true,
        })
    };
    let cv = opt(alt((
        pair(const_(), opt(preceded(space1, volatile()))),
        pair(volatile(), opt(preceded(space1, const_()))),
    )));
    map(cv, |opt_cv| {
        let (cv1, opt_cv2) = opt_cv.unwrap_or_default();
        let cv2 = opt_cv2.unwrap_or_default();
        ConstVolatile {
            is_const: cv1.is_const | cv2.is_const,
            is_volatile: cv1.is_volatile | cv2.is_volatile,
        }
    })(s)
}
//
/// CV qualifiers
#[derive(Default, Debug, PartialEq, Clone, Copy)]
struct ConstVolatile {
    is_const: bool,
    is_volatile: bool,
}

/// Parser recognizing primitive types inherited from C, which can have spaces
/// in their name
///
/// This is not a full parser for C++ primitive types, as most of them can be
/// parsed with the regular TypeOrValue logic, and we do not need to single out
/// primitives in our processing.
///
/// It will also accept a bunch of types that are invalid from the point of view
/// of the C++ grammar, such as "long long short", for the sake of simplicity:
/// clang should not normally emit these, so we don't really care about
/// processing them right.
fn legacy_primitive(s: &str) -> IResult<&str, IdExpression> {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::space1,
        combinator::{map, opt, recognize},
        multi::separated_list1,
        sequence::pair,
    };
    let signedness = recognize(pair(opt(tag("un")), tag("signed")));
    let size = alt((tag("short"), tag("long")));
    let base = alt((tag("int"), tag("char"), tag("double")));
    let anything = alt((signedness, size, base));
    let list = recognize(separated_list1(space1, anything));
    map(list, |id: &str| IdExpression {
        path: Default::default(),
        id: id.into(),
    })(s)
}

/// Parser recognizing types and some values, given an underlying identifier parser
///
/// This wraps either id_expression or legacy_primitive with extra logic
/// for CV qualifiers, pointers and references. Unfortunately, as a result of
/// the C++ grammar being the preposterous monster that it is, we cannot fully
/// decide at this layer of the parsing stack which of the id_expression or
/// legacy_primitive sub-parsers should be called.
///
/// Instead, we must reach the next delimiter character (e.g. ',' or '>' in
/// template parameter lists) before taking this decision.
fn type_or_value_impl(
    s: &str,
    inner_id: impl Fn(&str) -> IResult<&str, IdExpression>,
) -> IResult<&str, TypeOrValue> {
    use nom::{
        character::complete::{char, space0, space1},
        combinator::{map, opt},
        multi::{many0, many1_count},
        sequence::{pair, preceded, terminated, tuple},
    };
    let pointer_opt = preceded(pair(space0, char('*')), opt(preceded(space0, cv)));
    let pointer = map(pointer_opt, |cv| cv.unwrap_or_default());
    let pointers = many0(pointer);
    let num_refs_opt = opt(preceded(space1, many1_count(char('&'))));
    let tuple = tuple((
        opt(terminated(cv, space1)),
        inner_id,
        pointers,
        num_refs_opt,
    ));
    map(tuple, |(bottom_cv, bottom_type, pointers, num_refs_opt)| {
        TypeOrValue {
            bottom_cv: bottom_cv.unwrap_or_default(),
            bottom_type,
            pointers: pointers.into_boxed_slice(),
            num_references: num_refs_opt.unwrap_or_default() as u8,
        }
    })(s)
}

/// Parser recognizing types and some values, given a parser for the next delimiter
///
/// This resolves the type_or_value_impl ambiguity by checking out the next
/// delimiter, without consuming it.
fn type_or_value(
    s: &str,
    next_delimiter: impl FnMut(&str) -> IResult<&str, ()> + Copy,
) -> IResult<&str, TypeOrValue> {
    use nom::{branch::alt, combinator::peek, sequence::terminated};
    let id_expression = |s| type_or_value_impl(s, id_expression);
    let legacy_primitive = |s| type_or_value_impl(s, legacy_primitive);
    alt((
        terminated(id_expression, peek(next_delimiter)),
        terminated(legacy_primitive, peek(next_delimiter)),
    ))(s)
}

/// Output from type_or_value parsers
#[derive(Debug, PartialEq, Clone)]
struct TypeOrValue<'source> {
    /// CV qualifiers applying to the leftmost type
    bottom_cv: ConstVolatile,

    /// Leftmost type (may also match some values like true/false)
    bottom_type: IdExpression<'source>,

    /// Layers of pointer indirection (* const * volatile...)
    pointers: Box<[ConstVolatile]>,

    /// Number of final references between 0 (no reference) and 2 (rvalue)
    num_references: u8,
}

/// Parser recognizing template arguments
fn template_argument(s: &str) -> IResult<&str, TemplateArgument> {
    use nom::{
        branch::alt,
        character::complete::{char, space0},
        combinator::map,
        sequence::pair,
    };
    let integer_literal = map(integer_literal, TemplateArgument::Integer);
    fn delimiter(s: &str) -> IResult<&str, ()> {
        map(pair(space0, alt((char(','), char('>')))), std::mem::drop)(s)
    }
    let type_or_value = |s| type_or_value(s, delimiter);
    let type_or_value = map(type_or_value, TemplateArgument::TypeOrValue);
    alt((integer_literal, type_or_value))(s)
}
//
/// Template argument
#[derive(Debug, PartialEq, Clone)]
enum TemplateArgument<'source> {
    /// Integer literal
    Integer(i128),

    /// Type or value
    TypeOrValue(TypeOrValue<'source>),
}

/// Parser recognizing template parameters
fn template_parameters(s: &str) -> IResult<&str, Box<[TemplateArgument]>> {
    use nom::{
        character::complete::{char, space0},
        combinator::map,
        multi::separated_list1,
        sequence::{delimited, pair},
    };
    let arguments = separated_list1(pair(char(','), space0), template_argument);
    let parameters = delimited(char('<'), arguments, pair(space0, char('>')));
    map(parameters, |p| p.into_boxed_slice())(s)
}

/// Parser recognizing an identifier which may or may not be coupled with
/// template arguments, i.e. id or id<...>
fn templatable_id(s: &str) -> IResult<&str, TemplatableId> {
    use nom::{
        combinator::{map, opt},
        sequence::pair,
    };
    let parameters_or_empty = map(opt(template_parameters), |opt| opt.unwrap_or_default());
    map(pair(identifier, parameters_or_empty), |(id, parameters)| {
        TemplatableId { id, parameters }
    })(s)
}
//
/// Identifier which may or may not have template arguments
#[derive(Clone, Debug, PartialEq)]
struct TemplatableId<'source> {
    /// Identifier
    id: &'source str,

    /// Optional template parameters
    parameters: Box<[TemplateArgument<'source>]>,
}
//
impl<'source> From<&'source str> for TemplatableId<'source> {
    fn from(id: &'source str) -> Self {
        Self {
            id,
            parameters: Default::default(),
        }
    }
}

/// Parser for unqualified id-expressions
fn unqualified_id_expression(s: &str) -> IResult<&str, UnqualifiedId> {
    // FIXME: Accept all unqualified id-expressions. In addition to identifiers,
    //        these include...
    //        - Destructors: ~identifier
    //        - Templates: identifier<param...>
    //        - Operators, including conversion operators
    templatable_id(s)
}
//
type UnqualifiedId<'source> = TemplatableId<'source>;

/// Parser for id-expressions
fn id_expression(s: &str) -> IResult<&str, IdExpression> {
    use nom::{
        bytes::complete::tag,
        combinator::map,
        multi::many0,
        sequence::{pair, terminated},
    };
    let scope = terminated(templatable_id, tag("::"));
    let path = map(many0(scope), Vec::into_boxed_slice);
    let path_and_id = pair(path, unqualified_id_expression);
    map(path_and_id, |(path, id)| IdExpression { path, id })(s)
}
//
/// C++ id-expression
#[derive(Clone, Debug, PartialEq)]
struct IdExpression<'source> {
    /// Hierarchical scope (types or namespaces)
    path: Box<[TemplatableId<'source>]>,

    /// Unqualified id-expression
    id: UnqualifiedId<'source>,
}

/// Parser for clang's <unknown> C++ entity
fn unknown_entity(s: &str) -> IResult<&str, ()> {
    use nom::{bytes::complete::tag, combinator::map};
    map(tag("<unknown>"), std::mem::drop)(s)
}

/// Parser for clang lambda types "(lambda at <file path>:<line>:<col>)"
///
/// This will fail if the file path contains a ':' sign other than a
/// Windows-style disk designator at the start, because I have no idea how to
/// handle this inherent grammar ambiguity better...
fn lambda(s: &str) -> IResult<&str, Lambda> {
    use nom::{
        bytes::complete::{tag, take_until1},
        character::complete::{anychar, char, u32},
        combinator::{map, opt, recognize},
        sequence::{delimited, pair, separated_pair},
    };
    let disk_designator = recognize(pair(anychar, char(':')));
    let path_str = recognize(pair(opt(disk_designator), take_until1(":")));
    let path = map(path_str, Path::new);
    let location = separated_pair(u32, char(':'), u32);
    let file_location = separated_pair(path, char(':'), location);
    let lambda = map(file_location, |(file, location)| Lambda { file, location });
    delimited(tag("(lambda at "), lambda, char(')'))(s)
}
//
/// Lambda location description
#[derive(Clone, Debug, PartialEq)]
struct Lambda<'source> {
    /// In which file the lambda is declared
    file: &'source Path,

    /// Where exactly in the file
    location: (Line, Col),
}
//
type Line = u32;
type Col = u32;

/// Parser for C++ entities
fn entity(s: &str) -> IResult<&str, Option<CppEntity>> {
    use nom::{branch::alt, combinator::map};
    let id_expression = map(id_expression, |i| Some(CppEntity::IdExpression(i)));
    let unknown = map(unknown_entity, |()| None);
    let lambda = map(lambda, |l| Some(CppEntity::Lambda(l)));
    alt((id_expression, unknown, lambda))(s)
}
//
/// C++ entity description
#[derive(Clone, Debug, PartialEq)]
enum CppEntity<'source> {
    IdExpression(IdExpression<'source>),
    Lambda(Lambda<'source>),
}
