use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn names_scopes(c: &mut Criterion) {
    use cpparser::names::scopes;
    let name = |s| format!("names::scopes::{s}");

    let parser = EntityParser::new();

    let parse_nested_name_specifier =
        |s| scopes::nested_name_specifier(s, &atoms::identifier, &Path::new);
    c.bench_function(&name("nested_name_specifier/old/empty"), |b| {
        b.iter(|| parse_nested_name_specifier(black_box("")))
    });
    c.bench_function(&name("nested_name_specifier/old/root"), |b| {
        b.iter(|| parse_nested_name_specifier(black_box("::")))
    });
    c.bench_function(&name("nested_name_specifier/old/single"), |b| {
        b.iter(|| parse_nested_name_specifier(black_box("a::")))
    });
    c.bench_function(&name("nested_name_specifier/old/function"), |b| {
        b.iter(|| parse_nested_name_specifier(black_box("a()::")))
    });
    c.bench_function(&name("nested_name_specifier/old/fail"), |b| {
        b.iter(|| parse_nested_name_specifier(black_box("x")))
    });
    c.bench_function(&name("nested_name_specifier/new/empty"), |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("")))
    });
    c.bench_function(&name("nested_name_specifier/new/root"), |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("::")))
    });
    c.bench_function(&name("nested_name_specifier/new/single"), |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("a::")))
    });
    c.bench_function(&name("nested_name_specifier/new/function"), |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("a()::")))
    });
    c.bench_function(&name("nested_name_specifier/new/fail"), |b| {
        b.iter(|| parser.parse_nested_name_specifier(black_box("x")))
    });

    let parse_id_expression = |s| scopes::id_expression(s, &atoms::identifier, &Path::new);
    c.bench_function(&name("id_expression/old/single"), |b| {
        b.iter(|| parse_id_expression(black_box("a")))
    });
    c.bench_function(&name("id_expression/old/rooted"), |b| {
        b.iter(|| parse_id_expression(black_box("::a")))
    });
    c.bench_function(&name("id_expression/old/nested"), |b| {
        b.iter(|| parse_id_expression(black_box("a::b")))
    });
    c.bench_function(&name("id_expression/old/fail"), |b| {
        b.iter(|| parse_id_expression(black_box("*")))
    });
    c.bench_function(&name("id_expression/new/single"), |b| {
        b.iter(|| parser.parse_id_expression(black_box("a")))
    });
    c.bench_function(&name("id_expression/new/rooted"), |b| {
        b.iter(|| parser.parse_id_expression(black_box("::a")))
    });
    c.bench_function(&name("id_expression/new/nested"), |b| {
        b.iter(|| parser.parse_id_expression(black_box("a::b")))
    });
    c.bench_function(&name("id_expression/new/fail"), |b| {
        b.iter(|| parser.parse_id_expression(black_box("*")))
    });
}

criterion_group!(benches, names_scopes);
criterion_main!(benches);
