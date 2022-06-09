use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn values(c: &mut Criterion) {
    let parser = EntityParser::new();
    let parse_value_like = |s| parser.parse_value_like(s, true, true);

    // Literal
    c.bench_function("parse_value_like/1", |b| {
        b.iter(|| parse_value_like(black_box("1")))
    });

    // id-expression
    c.bench_function("parse_value_like/x", |b| {
        b.iter(|| parse_value_like(black_box("x")))
    });

    // Parenthesized value
    c.bench_function("parse_value_like/parenthesized", |b| {
        b.iter(|| parse_value_like(black_box("(1)")))
    });

    // Unary operator
    c.bench_function("parse_value_like/--x", |b| {
        b.iter(|| parse_value_like(black_box("--x")))
    });

    // new-expression
    c.bench_function("parse_value_like/new_expression", |b| {
        b.iter(|| parse_value_like(black_box("new T")))
    });

    // Function call
    c.bench_function("parse_value_like/function_call", |b| {
        b.iter(|| parse_value_like(black_box("f()")))
    });

    // Binary operator
    c.bench_function("parse_value_like/binary_op", |b| {
        b.iter(|| parse_value_like(black_box("1+1")))
    });

    // Ternary operator
    c.bench_function("parse_value_like/ternary_op", |b| {
        b.iter(|| parse_value_like(black_box("1?2:3")))
    });

    // Member access
    c.bench_function("parse_value_like/member", |b| {
        b.iter(|| parse_value_like(black_box("a.b")))
    });

    // Array indexing
    c.bench_function("parse_value_like/array", |b| {
        b.iter(|| parse_value_like(black_box("a[1]")))
    });

    // Postfix operator
    c.bench_function("parse_value_like/postfix", |b| {
        b.iter(|| parse_value_like(black_box("a++")))
    });

    // Not a value
    c.bench_function("parse_value_like/fail", |b| {
        b.iter(|| parse_value_like(black_box("#")))
    });
}

criterion_group!(benches, values);
criterion_main!(benches);
