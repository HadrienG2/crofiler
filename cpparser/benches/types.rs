use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn types(c: &mut Criterion) {
    let mut parser = EntityParser::new();

    // Basic type parsing
    c.bench_function("parse_type_like/double", |b| {
        b.iter(|| parser.parse_type_like(black_box("double")))
    });

    // GNU-style attributes
    c.bench_function("parse_type_like/aligned_double", |b| {
        b.iter(|| parser.parse_type_like(black_box("__attribute__((aligned)) double")))
    });

    // Declarator
    c.bench_function("parse_type_like/double&", |b| {
        b.iter(|| parser.parse_type_like(black_box("double&")))
    });

    // Failing case
    c.bench_function("parse_type_like/fail", |b| {
        b.iter(|| parser.parse_type_like(black_box("1")))
    });
}

criterion_group!(benches, types);
criterion_main!(benches);
