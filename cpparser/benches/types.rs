use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn types(c: &mut Criterion) {
    use cpparser::types;
    let name = |s| format!("types::{s}");

    let parser = EntityParser::new();

    let parse_type_like = |s| types::type_like(s, &atoms::identifier, &Path::new);

    // Basic type parsing
    c.bench_function(&name("type_like/old/double"), |b| {
        b.iter(|| parse_type_like(black_box("double")))
    });
    c.bench_function(&name("type_like/new/double"), |b| {
        b.iter(|| parser.parse_type_like(black_box("double")))
    });

    // GNU-style attributes
    c.bench_function(&name("type_like/old/aligned_double"), |b| {
        b.iter(|| parse_type_like(black_box("__attribute__((aligned)) double")))
    });
    c.bench_function(&name("type_like/new/aligned_double"), |b| {
        b.iter(|| parser.parse_type_like(black_box("__attribute__((aligned)) double")))
    });

    // Declarator
    c.bench_function(&name("type_like/old/double&"), |b| {
        b.iter(|| parse_type_like(black_box("double&")))
    });
    c.bench_function(&name("type_like/new/double&"), |b| {
        b.iter(|| parser.parse_type_like(black_box("double&")))
    });

    // Failing case
    c.bench_function(&name("type_like/old/fail"), |b| {
        b.iter(|| parse_type_like(black_box("1")))
    });
    c.bench_function(&name("type_like/new/fail"), |b| {
        b.iter(|| parser.parse_type_like(black_box("1")))
    });
}

criterion_group!(benches, types);
criterion_main!(benches);
