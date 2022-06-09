use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn types_specifiers(c: &mut Criterion) {
    let parser = EntityParser::new();

    // Legacy primitive branch (most common parse)
    c.bench_function("parse_type_specifier/double", |b| {
        b.iter(|| parser.parse_type_specifier(black_box("double")))
    });

    // Normal branch
    c.bench_function("parse_type_specifier/a", |b| {
        b.iter(|| parser.parse_type_specifier(black_box("a")))
    });

    // CV qualifiers
    c.bench_function("parse_type_specifier/const T", |b| {
        b.iter(|| parser.parse_type_specifier(black_box("const T")))
    });
    c.bench_function("parse_type_specifier/T const", |b| {
        b.iter(|| parser.parse_type_specifier(black_box("T const")))
    });

    // Keywords
    c.bench_function("parse_type_specifier/typename T", |b| {
        b.iter(|| parser.parse_type_specifier(black_box("typename T")))
    });

    // Failing case
    c.bench_function("parse_type_specifier/fail", |b| {
        b.iter(|| parser.parse_type_specifier(black_box("1")))
    });
}

criterion_group!(benches, types_specifiers);
criterion_main!(benches);
