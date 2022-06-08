use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn templates(c: &mut Criterion) {
    use cpparser::templates;
    let name = |s| format!("templates::{s}");

    let parser = EntityParser::new();
    let parse_template_parameters =
        |s| templates::template_parameters(s, &atoms::identifier, &Path::new);

    // Empty parameter set
    c.bench_function(&name("template_parameters/old/<>"), |b| {
        b.iter(|| parse_template_parameters(black_box("<>")))
    });
    c.bench_function(&name("template_parameters/new/<>"), |b| {
        b.iter(|| parser.parse_template_parameters(black_box("<>")))
    });

    // Type parameter
    c.bench_function(&name("template_parameters/old/<double>"), |b| {
        b.iter(|| parse_template_parameters(black_box("<double>")))
    });
    c.bench_function(&name("template_parameters/new/<double>"), |b| {
        b.iter(|| parser.parse_template_parameters(black_box("<double>")))
    });

    // Value parameter
    c.bench_function(&name("template_parameters/old/<1>"), |b| {
        b.iter(|| parse_template_parameters(black_box("<1>")))
    });
    c.bench_function(&name("template_parameters/new/<1>"), |b| {
        b.iter(|| parser.parse_template_parameters(black_box("<1>")))
    });

    // Failing parse
    c.bench_function(&name("template_parameters/old/fail"), |b| {
        b.iter(|| parse_template_parameters(black_box("x")))
    });
    c.bench_function(&name("template_parameters/new/fail"), |b| {
        b.iter(|| parser.parse_template_parameters(black_box("x")))
    });
}

criterion_group!(benches, templates);
criterion_main!(benches);
