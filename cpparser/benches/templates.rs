use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn templates(c: &mut Criterion) {
    let parser = EntityParser::new();

    // Empty parameter set
    c.bench_function("parse_template_parameters/<>", |b| {
        b.iter(|| parser.parse_template_parameters(black_box("<>")))
    });

    // Type parameter
    c.bench_function("template_parameters/<double>", |b| {
        b.iter(|| parser.parse_template_parameters(black_box("<double>")))
    });

    // Value parameter
    c.bench_function("template_parameters/<1>", |b| {
        b.iter(|| parser.parse_template_parameters(black_box("<1>")))
    });

    // Failing parse
    c.bench_function("template_parameters/fail", |b| {
        b.iter(|| parser.parse_template_parameters(black_box("x")))
    });
}

criterion_group!(benches, templates);
criterion_main!(benches);
