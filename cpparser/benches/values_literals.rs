use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn values_literals(c: &mut Criterion) {
    use cpparser::values::literals;
    let name = |s| format!("values::literals::{s}");

    // Literal parsing: old-style, without interning...
    let parse_literal = |s| literals::literal(s, &atoms::identifier);
    c.bench_function(&name("literal/old/1"), |b| {
        b.iter(|| parse_literal(black_box("1")))
    });
    c.bench_function(&name("literal/old/1U"), |b| {
        b.iter(|| parse_literal(black_box("1U")))
    });
    c.bench_function(&name("literal/old/1ULL"), |b| {
        b.iter(|| parse_literal(black_box("1ULL")))
    });
    c.bench_function(&name("literal/old/1_x"), |b| {
        b.iter(|| parse_literal(black_box("1_x")))
    });
    //
    c.bench_function(&name("literal/old/'x'"), |b| {
        b.iter(|| parse_literal(black_box("1")))
    });
    c.bench_function(&name("literal/old/'\\n'"), |b| {
        b.iter(|| parse_literal(black_box("'\\n'")))
    });
    //
    c.bench_function(&name("literal/old/fail"), |b| {
        b.iter(|| parse_literal(black_box("fail")))
    });

    // ...and new-style with interning
    // TODO: Also measure cold cache perf & retrieval perf
    let parser = EntityParser::new();
    c.bench_function(&name("literal/new/1"), |b| {
        b.iter(|| parser.parse_literal(black_box("1")))
    });
    c.bench_function(&name("literal/new/1U"), |b| {
        b.iter(|| parser.parse_literal(black_box("1U")))
    });
    c.bench_function(&name("literal/new/1ULL"), |b| {
        b.iter(|| parser.parse_literal(black_box("1ULL")))
    });
    c.bench_function(&name("literal/new/1_x"), |b| {
        b.iter(|| parser.parse_literal(black_box("1_x")))
    });
    //
    c.bench_function(&name("literal/new/'x'"), |b| {
        b.iter(|| parser.parse_literal(black_box("1")))
    });
    c.bench_function(&name("literal/new/'\\n'"), |b| {
        b.iter(|| parser.parse_literal(black_box("'\\n'")))
    });
    //
    c.bench_function(&name("literal/new/fail"), |b| {
        b.iter(|| parser.parse_literal(black_box("fail")))
    });
}

criterion_group!(benches, values_literals);
criterion_main!(benches);
