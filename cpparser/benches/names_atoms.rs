use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn names_atoms(c: &mut Criterion) {
    let parser = EntityParser::new();

    // Identifier parsing
    c.bench_function("parse_identifier/small", |b| {
        b.iter(|| parser.parse_identifier(black_box("a")))
    });
    c.bench_function("parse_identifier/large", |b| {
        b.iter(|| parser.parse_identifier(black_box("ItShouldntGetLongerThanThis")))
    });
    c.bench_function("parse_identifier/fail", |b| {
        b.iter(|| parser.parse_identifier(black_box("6")))
    });

    // Shortest keyword we parse
    let new = EntityParser::keyword_parser("new");
    c.bench_function("parse_keyword/new/pass", |b| {
        b.iter(|| new(black_box("new")))
    });
    c.bench_function("parse_keyword/new/fail/early", |b| {
        b.iter(|| new(black_box("typename")))
    });
    c.bench_function("parse_keyword/new/fail/late", |b| {
        b.iter(|| new(black_box("newer")))
    });

    // Longest keyword we parse
    let typename = EntityParser::keyword_parser("typename");
    c.bench_function("parse_keyword/typename/pass", |b| {
        b.iter(|| typename(black_box("typename")))
    });
    c.bench_function("parse_keyword/typename/fail/early", |b| {
        b.iter(|| typename(black_box("new")))
    });
    c.bench_function("parse_keyword/typename/fail/late", |b| {
        b.iter(|| typename(black_box("typenamed")))
    });

    // Keyword set
    let keywords = EntityParser::keywords_parser([
        "int", "char", "double", "unsigned", "short", "long", "signed",
    ]);
    c.bench_function("parse_keywords/pass/first", |b| {
        b.iter(|| keywords(black_box("int")))
    });
    c.bench_function("parse_keywords/pass/last", |b| {
        b.iter(|| keywords(black_box("signed")))
    });
    c.bench_function("parse_keywords/fail", |b| {
        b.iter(|| keywords(black_box("x")))
    });
}

criterion_group!(benches, names_atoms);
criterion_main!(benches);
