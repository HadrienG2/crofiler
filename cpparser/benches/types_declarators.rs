use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn types_declarators(c: &mut Criterion) {
    use cpparser::types::declarators;
    let name = |s| format!("types::declarators::{s}");

    let parser = EntityParser::new();

    let parse_declarator = |s| declarators::declarator(s, &atoms::identifier, &Path::new);

    // Empty declarator
    c.bench_function(&name("declarator/old/empty"), |b| {
        b.iter(|| parse_declarator(black_box("")))
    });
    c.bench_function(&name("declarator/new/empty"), |b| {
        b.iter(|| parser.parse_declarator(black_box("")))
    });

    // Reference
    c.bench_function(&name("declarator/old/&"), |b| {
        b.iter(|| parse_declarator(black_box("&")))
    });
    c.bench_function(&name("declarator/new/&"), |b| {
        b.iter(|| parser.parse_declarator(black_box("&")))
    });

    // Basic pointer
    c.bench_function(&name("declarator/old/*"), |b| {
        b.iter(|| parse_declarator(black_box("*")))
    });
    c.bench_function(&name("declarator/new/*"), |b| {
        b.iter(|| parser.parse_declarator(black_box("*")))
    });

    // Function
    c.bench_function(&name("declarator/old/()"), |b| {
        b.iter(|| parse_declarator(black_box("()")))
    });
    c.bench_function(&name("declarator/new/()"), |b| {
        b.iter(|| parser.parse_declarator(black_box("()")))
    });

    // Parenthesized
    c.bench_function(&name("declarator/old/(&)"), |b| {
        b.iter(|| parse_declarator(black_box("(&)")))
    });
    c.bench_function(&name("declarator/new/(&)"), |b| {
        b.iter(|| parser.parse_declarator(black_box("(&)")))
    });

    // Array
    c.bench_function(&name("declarator/old/[]"), |b| {
        b.iter(|| parse_declarator(black_box("[]")))
    });
    c.bench_function(&name("declarator/new/[]"), |b| {
        b.iter(|| parser.parse_declarator(black_box("[]")))
    });

    // Member pointer
    c.bench_function(&name("declarator/old/T::*"), |b| {
        b.iter(|| parse_declarator(black_box("T::*")))
    });
    c.bench_function(&name("declarator/new/T::*"), |b| {
        b.iter(|| parser.parse_declarator(black_box("T::*")))
    });
}

criterion_group!(benches, types_declarators);
criterion_main!(benches);
