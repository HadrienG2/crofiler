use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn operators_usage(c: &mut Criterion) {
    use cpparser::operators::usage;
    let name = |s| format!("operators::usage::{s}");
    let parser = EntityParser::new();

    // Increment/decrement operator
    c.bench_function(&name("increment_decrement/++"), |b| {
        b.iter(|| EntityParser::parse_increment_decrement(black_box("++")))
    });
    c.bench_function(&name("increment_decrement/--"), |b| {
        b.iter(|| EntityParser::parse_increment_decrement(black_box("--")))
    });
    c.bench_function(&name("increment_decrement/fail"), |b| {
        b.iter(|| EntityParser::parse_increment_decrement(black_box("x")))
    });

    // Unary prefix operator for expressions
    let parse_unary_expr_prefix = |s| usage::unary_expr_prefix(s, &atoms::identifier, &Path::new);
    //
    c.bench_function(&name("unary_expr_prefix/old/cast"), |b| {
        b.iter(|| parse_unary_expr_prefix(black_box("(T)")))
    });
    c.bench_function(&name("unary_expr_prefix/new/cast"), |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("(T)")))
    });
    //
    c.bench_function(&name("unary_expr_prefix/old/++"), |b| {
        b.iter(|| parse_unary_expr_prefix(black_box("++")))
    });
    c.bench_function(&name("unary_expr_prefix/new/++"), |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("++")))
    });
    //
    c.bench_function(&name("unary_expr_prefix/old/+"), |b| {
        b.iter(|| parse_unary_expr_prefix(black_box("+")))
    });
    c.bench_function(&name("unary_expr_prefix/old/!"), |b| {
        b.iter(|| parse_unary_expr_prefix(black_box("!")))
    });
    c.bench_function(&name("unary_expr_prefix/new/+"), |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("+")))
    });
    c.bench_function(&name("unary_expr_prefix/new/!"), |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("!")))
    });
    //
    c.bench_function(&name("unary_expr_prefix/old/delete"), |b| {
        b.iter(|| parse_unary_expr_prefix(black_box("delete")))
    });
    c.bench_function(&name("unary_expr_prefix/new/delete"), |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("delete")))
    });
    //
    c.bench_function(&name("unary_expr_prefix/old/co_await"), |b| {
        b.iter(|| parse_unary_expr_prefix(black_box("co_await")))
    });
    c.bench_function(&name("unary_expr_prefix/new/co_await"), |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("co_await")))
    });
    //
    c.bench_function(&name("unary_expr_prefix/old/fail"), |b| {
        b.iter(|| parse_unary_expr_prefix(black_box("x")))
    });
    c.bench_function(&name("unary_expr_prefix/new/fail"), |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("x")))
    });

    // Binary operator between two expressions
    let parse_binary_expr_middle = EntityParser::parse_binary_expr_middle::<true, true>;
    c.bench_function(&name("binary_expr_middle/+"), |b| {
        b.iter(|| parse_binary_expr_middle(black_box("+")))
    });
    c.bench_function(&name("binary_expr_middle/=="), |b| {
        b.iter(|| parse_binary_expr_middle(black_box("==")))
    });
    c.bench_function(&name("binary_expr_middle/<=>"), |b| {
        b.iter(|| parse_binary_expr_middle(black_box("<=>")))
    });
    c.bench_function(&name("binary_expr_middle/fail"), |b| {
        b.iter(|| parse_binary_expr_middle(black_box("x")))
    });

    // New expression, i.e. usage of the new operator
    let parse_new_expression = |s| usage::new_expression(s, &atoms::identifier, &Path::new);
    //
    c.bench_function(&name("new_expression/old/basic"), |b| {
        b.iter(|| parse_new_expression(black_box("new T")))
    });
    c.bench_function(&name("new_expression/new/basic"), |b| {
        b.iter(|| parser.parse_new_expression(black_box("new T")))
    });
    //
    c.bench_function(&name("new_expression/old/rooted"), |b| {
        b.iter(|| parse_new_expression(black_box("::new T")))
    });
    c.bench_function(&name("new_expression/new/rooted"), |b| {
        b.iter(|| parser.parse_new_expression(black_box("::new T")))
    });
    //
    c.bench_function(&name("new_expression/old/placement"), |b| {
        b.iter(|| parse_new_expression(black_box("new() T")))
    });
    c.bench_function(&name("new_expression/new/placement"), |b| {
        b.iter(|| parser.parse_new_expression(black_box("new() T")))
    });
    //
    c.bench_function(&name("new_expression/old/params"), |b| {
        b.iter(|| parse_new_expression(black_box("new T()")))
    });
    c.bench_function(&name("new_expression/new/params"), |b| {
        b.iter(|| parser.parse_new_expression(black_box("new T()")))
    });
    //
    c.bench_function(&name("new_expression/old/fail"), |b| {
        b.iter(|| parse_new_expression(black_box("delete")))
    });
    c.bench_function(&name("new_expression/new/fail"), |b| {
        b.iter(|| parser.parse_new_expression(black_box("delete")))
    });
}

criterion_group!(benches, operators_usage);
criterion_main!(benches);
