use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn names_scopes(c: &mut Criterion) {
    let mut parser = EntityParser::new();

    c.bench_function("parse_nested_name_specifier/empty", |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("")))
    });
    c.bench_function("parse_nested_name_specifier/root", |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("::")))
    });
    c.bench_function("parse_nested_name_specifier/single", |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("a::")))
    });
    c.bench_function("parse_nested_name_specifier/function", |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("a()::")))
    });
    c.bench_function("parse_nested_name_specifier/fail", |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("x")))
    });

    c.bench_function("parse_id_expression/single", |b| {
        b.iter(|| parser.parse_id_expression(black_box("a")))
    });
    c.bench_function("parse_id_expression/rooted", |b| {
        b.iter(|| parser.parse_id_expression(black_box("::a")))
    });
    c.bench_function("parse_id_expression/nested", |b| {
        b.iter(|| parser.parse_id_expression(black_box("a::b")))
    });
    c.bench_function("parse_id_expression/fail", |b| {
        b.iter(|| parser.parse_id_expression(black_box("*")))
    });
}

criterion_group!(benches, names_scopes);
criterion_main!(benches);
