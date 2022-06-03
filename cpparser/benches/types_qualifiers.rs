use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn types_qualifiers(c: &mut Criterion) {
    let name = |s| format!("types::qualifiers::{s}");

    // The only cv qualifier we really care about is presence or absence of const
    c.bench_function(&name("cv/const"), |b| {
        b.iter(|| EntityParser::parse_cv(black_box("const")))
    });
    c.bench_function(&name("cv/none"), |b| {
        b.iter(|| EntityParser::parse_cv(black_box("x")))
    });

    // On the other hand, we do care about all reference types
    c.bench_function(&name("reference/lvalue"), |b| {
        b.iter(|| EntityParser::parse_reference(black_box("&")))
    });
    c.bench_function(&name("reference/rvalue"), |b| {
        b.iter(|| EntityParser::parse_reference(black_box("&&")))
    });
    c.bench_function(&name("reference/none"), |b| {
        b.iter(|| EntityParser::parse_reference(black_box("x")))
    });
}

criterion_group!(benches, types_qualifiers);
criterion_main!(benches);
