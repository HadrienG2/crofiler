use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn operators_usage(c: &mut Criterion) {
    let parser = EntityParser::new();

    // Increment/decrement operator
    c.bench_function("parse_increment_decrement/++", |b| {
        b.iter(|| EntityParser::parse_increment_decrement(black_box("++")))
    });
    c.bench_function("parse_increment_decrement/--", |b| {
        b.iter(|| EntityParser::parse_increment_decrement(black_box("--")))
    });
    c.bench_function("parse_increment_decrement/fail", |b| {
        b.iter(|| EntityParser::parse_increment_decrement(black_box("x")))
    });

    // Unary prefix operator for expressions
    c.bench_function("parse_unary_expr_prefix/cast", |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("(T)")))
    });
    //
    c.bench_function("parse_unary_expr_prefix/++", |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("++")))
    });
    //
    c.bench_function("parse_unary_expr_prefix/+", |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("+")))
    });
    c.bench_function("parse_unary_expr_prefix/!", |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("!")))
    });
    //
    c.bench_function("parse_unary_expr_prefix/delete", |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("delete")))
    });
    //
    c.bench_function("parse_unary_expr_prefix/co_await", |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("co_await")))
    });
    //
    c.bench_function("parse_unary_expr_prefix/fail", |b| {
        b.iter(|| parser.parse_unary_expr_prefix(black_box("x")))
    });

    // Binary operator between two expressions
    let parse_binary_expr_middle = |s| EntityParser::parse_binary_expr_middle(s, true, true);
    //
    c.bench_function("parse_binary_expr_middle/+", |b| {
        b.iter(|| parse_binary_expr_middle(black_box("+")))
    });
    c.bench_function("parse_binary_expr_middle/==", |b| {
        b.iter(|| parse_binary_expr_middle(black_box("==")))
    });
    c.bench_function("parse_binary_expr_middle/<=>", |b| {
        b.iter(|| parse_binary_expr_middle(black_box("<=>")))
    });
    c.bench_function("parse_binary_expr_middle/fail", |b| {
        b.iter(|| parse_binary_expr_middle(black_box("x")))
    });

    // New expression, i.e. usage of the new operator
    c.bench_function("parse_new_expression/basic", |b| {
        b.iter(|| parser.parse_new_expression(black_box("new T")))
    });
    //
    c.bench_function("parse_new_expression/rooted", |b| {
        b.iter(|| parser.parse_new_expression(black_box("::new T")))
    });
    //
    c.bench_function("parse_new_expression/placement", |b| {
        b.iter(|| parser.parse_new_expression(black_box("new() T")))
    });
    //
    c.bench_function("parse_new_expression/params", |b| {
        b.iter(|| parser.parse_new_expression(black_box("new T()")))
    });
    //
    c.bench_function("parse_new_expression/fail", |b| {
        b.iter(|| parser.parse_new_expression(black_box("delete")))
    });
}

criterion_group!(benches, operators_usage);
criterion_main!(benches);
