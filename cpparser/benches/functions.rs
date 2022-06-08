use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn functions(c: &mut Criterion) {
    use cpparser::functions;
    let name = |s| format!("functions::{s}");
    let parser = EntityParser::new();

    // Pure function call
    let parse_function_call = |s| functions::function_call(s, &atoms::identifier, &Path::new);
    //
    c.bench_function(&name("function_call/old/basic"), |b| {
        b.iter(|| parse_function_call(black_box("()")))
    });
    c.bench_function(&name("function_call/new/basic"), |b| {
        b.iter(|| parser.parse_function_call(black_box("()")))
    });
    //
    c.bench_function(&name("function_call/old/fail"), |b| {
        b.iter(|| parse_function_call(black_box("x")))
    });
    c.bench_function(&name("function_call/new/fail"), |b| {
        b.iter(|| parser.parse_function_call(black_box("x")))
    });

    // ---

    // Function signatures
    let parse_function_signature =
        |s| functions::function_signature(s, &atoms::identifier, &Path::new);

    // Minimal
    c.bench_function(&name("function_signature/old/minimal"), |b| {
        b.iter(|| parse_function_signature(black_box("()")))
    });
    c.bench_function(&name("function_signature/new/minimal"), |b| {
        b.iter(|| parser.parse_function_signature(black_box("()")))
    });

    // CV qualifiers
    c.bench_function(&name("function_signature/old/const"), |b| {
        b.iter(|| parse_function_signature(black_box("() const")))
    });
    c.bench_function(&name("function_signature/new/const"), |b| {
        b.iter(|| parser.parse_function_signature(black_box("() const")))
    });

    // Reference qualifiers
    c.bench_function(&name("function_signature/old/&"), |b| {
        b.iter(|| parse_function_signature(black_box("() &")))
    });
    c.bench_function(&name("function_signature/new/&"), |b| {
        b.iter(|| parser.parse_function_signature(black_box("() &")))
    });

    // Noexcept qualifiers
    c.bench_function(&name("function_signature/old/noexcept"), |b| {
        b.iter(|| parse_function_signature(black_box("() noexcept")))
    });
    c.bench_function(&name("function_signature/old/noexcept_true"), |b| {
        b.iter(|| parse_function_signature(black_box("() noexcept(true)")))
    });
    c.bench_function(&name("function_signature/new/noexcept"), |b| {
        b.iter(|| parser.parse_function_signature(black_box("() noexcept")))
    });
    c.bench_function(&name("function_signature/new/noexcept_true"), |b| {
        b.iter(|| parser.parse_function_signature(black_box("() noexcept(true)")))
    });

    // Trailing return type
    c.bench_function(&name("function_signature/old/trailing_return"), |b| {
        b.iter(|| parse_function_signature(black_box("() -> T")))
    });
    c.bench_function(&name("function_signature/new/trailing_return"), |b| {
        b.iter(|| parser.parse_function_signature(black_box("() -> T")))
    });

    // Failed parse
    c.bench_function(&name("function_signature/old/fail"), |b| {
        b.iter(|| parse_function_signature(black_box("x")))
    });
    c.bench_function(&name("function_signature/new/fail"), |b| {
        b.iter(|| parser.parse_function_signature(black_box("x")))
    });
}

criterion_group!(benches, functions);
criterion_main!(benches);
