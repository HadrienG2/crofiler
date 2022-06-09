use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn types_declarators(c: &mut Criterion) {
    let parser = EntityParser::new();

    // Empty declarator
    c.bench_function("parse_declarator/empty", |b| {
        b.iter(|| parser.parse_declarator(black_box("")))
    });

    // Reference
    c.bench_function("parse_declarator/&", |b| {
        b.iter(|| parser.parse_declarator(black_box("&")))
    });

    // Basic pointer
    c.bench_function("parse_declarator/*", |b| {
        b.iter(|| parser.parse_declarator(black_box("*")))
    });

    // Function
    c.bench_function("parse_declarator/()", |b| {
        b.iter(|| parser.parse_declarator(black_box("()")))
    });

    // Parenthesized
    c.bench_function("parse_declarator/(&)", |b| {
        b.iter(|| parser.parse_declarator(black_box("(&)")))
    });

    // Array
    c.bench_function("parse_declarator/[]", |b| {
        b.iter(|| parser.parse_declarator(black_box("[]")))
    });

    // Member pointer
    c.bench_function("parse_declarator/T::*", |b| {
        b.iter(|| parser.parse_declarator(black_box("T::*")))
    });

    // Failing case
    c.bench_function("parse_declarator/fail", |b| {
        b.iter(|| parser.parse_declarator(black_box("1")))
    });
}

criterion_group!(benches, types_declarators);
criterion_main!(benches);
