use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn names_unqualified(c: &mut Criterion) {
    use cpparser::names::unqualified;
    let name = |s| format!("names::unqualified::{s}");

    let parse_unqualified_id = |s| unqualified::unqualified_id(s, &atoms::identifier, &Path::new);
    let parser = EntityParser::new();

    // Various flavors of named entity
    c.bench_function(&name("unqualified_id/old/a"), |b| {
        b.iter(|| parse_unqualified_id(black_box("a")))
    });
    c.bench_function(&name("unqualified_id/old/~a"), |b| {
        b.iter(|| parse_unqualified_id(black_box("~a")))
    });
    c.bench_function(&name("unqualified_id/old/a<>"), |b| {
        b.iter(|| parse_unqualified_id(black_box("a<>")))
    });
    c.bench_function(&name("unqualified_id/new/a"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("a")))
    });
    c.bench_function(&name("unqualified_id/new/~a"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("~a")))
    });
    c.bench_function(&name("unqualified_id/new/a<>"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("a<>")))
    });

    // Cheap and expensive operator overload
    c.bench_function(&name("unqualified_id/old/operator+"), |b| {
        b.iter(|| parse_unqualified_id(black_box("operator+")))
    });
    c.bench_function(&name("unqualified_id/old/operator int"), |b| {
        b.iter(|| parse_unqualified_id(black_box("operator int")))
    });
    c.bench_function(&name("unqualified_id/new/operator+"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("operator+")))
    });
    c.bench_function(&name("unqualified_id/new/operator int"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("operator int")))
    });

    // Decltype expression
    c.bench_function(&name("unqualified_id/old/decltype"), |b| {
        b.iter(|| parse_unqualified_id(black_box("decltype(1)")))
    });
    c.bench_function(&name("unqualified_id/new/decltype"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("decltype(1)")))
    });

    // Anonymous entities
    c.bench_function(&name("unqualified_id/old/lambda"), |b| {
        b.iter(|| parse_unqualified_id(black_box("(lambda at /x.hpp:1:2)")))
    });
    c.bench_function(&name("unqualified_id/old/anonymous"), |b| {
        b.iter(|| parse_unqualified_id(black_box("(anonymous namespace)")))
    });
    c.bench_function(&name("unqualified_id/new/lambda"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("(lambda at /x.hpp:1:2)")))
    });
    c.bench_function(&name("unqualified_id/new/anonymous"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("(anonymous namespace)")))
    });

    // Failure
    c.bench_function(&name("unqualified_id/old/fail"), |b| {
        b.iter(|| parse_unqualified_id(black_box("#")))
    });
    c.bench_function(&name("unqualified_id/new/fail"), |b| {
        b.iter(|| parser.parse_unqualified_id(black_box("#")))
    });
}

criterion_group!(benches, names_unqualified);
criterion_main!(benches);
