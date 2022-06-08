use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn types_specifiers(c: &mut Criterion) {
    use cpparser::types::specifiers;
    let name = |s| format!("types::specifiers::{s}");

    let parser = EntityParser::new();

    let parse_type_specifier = |s| specifiers::type_specifier(s, &atoms::identifier, &Path::new);

    // Legacy primitive branch (most common parse)
    c.bench_function(&name("type_specifier/old/double"), |b| {
        b.iter(|| parse_type_specifier(black_box("double")))
    });
    c.bench_function(&name("type_specifier/new/double"), |b| {
        b.iter(|| parser.parse_type_specifier(black_box("double")))
    });

    // Normal branch
    c.bench_function(&name("type_specifier/old/a"), |b| {
        b.iter(|| parse_type_specifier(black_box("a")))
    });
    c.bench_function(&name("type_specifier/new/a"), |b| {
        b.iter(|| parser.parse_type_specifier(black_box("a")))
    });

    // CV qualifiers
    c.bench_function(&name("type_specifier/old/const T"), |b| {
        b.iter(|| parse_type_specifier(black_box("const T")))
    });
    c.bench_function(&name("type_specifier/old/T const"), |b| {
        b.iter(|| parse_type_specifier(black_box("T const")))
    });
    c.bench_function(&name("type_specifier/new/const T"), |b| {
        b.iter(|| parser.parse_type_specifier(black_box("const T")))
    });
    c.bench_function(&name("type_specifier/new/T const"), |b| {
        b.iter(|| parser.parse_type_specifier(black_box("T const")))
    });

    // Keywords
    c.bench_function(&name("type_specifier/old/typename T"), |b| {
        b.iter(|| parse_type_specifier(black_box("typename T")))
    });
    c.bench_function(&name("type_specifier/new/typename T"), |b| {
        b.iter(|| parser.parse_type_specifier(black_box("typename T")))
    });
}

criterion_group!(benches, types_specifiers);
criterion_main!(benches);
