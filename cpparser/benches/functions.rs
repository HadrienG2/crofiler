use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn functions(c: &mut Criterion) {
    let mut parser = EntityParser::new();

    // Pure function call
    c.bench_function("parse_function_call/basic", |b| {
        b.iter(|| parser.parse_function_call(black_box("()")))
    });
    c.bench_function("parse_function_call/fail", |b| {
        b.iter(|| parser.parse_function_call(black_box("x")))
    });

    // Minimal
    c.bench_function("parse_function_signature/minimal", |b| {
        b.iter(|| parser.parse_function_signature(black_box("()")))
    });

    // CV qualifiers
    c.bench_function("parse_function_signature/const", |b| {
        b.iter(|| parser.parse_function_signature(black_box("() const")))
    });

    // Reference qualifiers
    c.bench_function("parse_function_signature/&", |b| {
        b.iter(|| parser.parse_function_signature(black_box("() &")))
    });

    // Noexcept qualifiers
    c.bench_function("parse_function_signature/noexcept", |b| {
        b.iter(|| parser.parse_function_signature(black_box("() noexcept")))
    });
    c.bench_function("parse_function_signature/noexcept_true", |b| {
        b.iter(|| parser.parse_function_signature(black_box("() noexcept(true)")))
    });

    // Trailing return type
    c.bench_function("parse_function_signature/trailing_return", |b| {
        b.iter(|| parser.parse_function_signature(black_box("() -> T")))
    });

    // Failed parse
    c.bench_function("parse_function_signature/fail", |b| {
        b.iter(|| parser.parse_function_signature(black_box("x")))
    });
}

criterion_group!(benches, functions);
criterion_main!(benches);
