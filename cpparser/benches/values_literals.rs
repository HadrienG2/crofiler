use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn values_literals(c: &mut Criterion) {
    let mut parser = EntityParser::new();

    // Integer literals
    c.bench_function("parse_literal/1", |b| {
        b.iter(|| parser.parse_literal(black_box("1")))
    });
    c.bench_function("parse_literal/1U", |b| {
        b.iter(|| parser.parse_literal(black_box("1U")))
    });
    c.bench_function("parse_literal/1ULL", |b| {
        b.iter(|| parser.parse_literal(black_box("1ULL")))
    });
    c.bench_function("parse_literal/1_x", |b| {
        b.iter(|| parser.parse_literal(black_box("1_x")))
    });

    // Character literals
    c.bench_function("parse_literal/'x'", |b| {
        b.iter(|| parser.parse_literal(black_box("1")))
    });
    c.bench_function("parse_literal/'\\n'", |b| {
        b.iter(|| parser.parse_literal(black_box("'\\n'")))
    });

    // Failed parse
    c.bench_function("parse_literal/fail", |b| {
        b.iter(|| parser.parse_literal(black_box("fail")))
    });
}

criterion_group!(benches, values_literals);
criterion_main!(benches);
