use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn values(c: &mut Criterion) {
    use cpparser::values;
    let name = |s| format!("values::{s}");

    let parser = EntityParser::new();
    let parse_value_like_old =
        |s| values::value_like(s, &atoms::identifier, &Path::new, true, true);
    let parse_value_like_new = |s| parser.parse_value_like(s, true, true);

    // Literal
    c.bench_function(&name("value_like/old/1"), |b| {
        b.iter(|| parse_value_like_old(black_box("1")))
    });
    c.bench_function(&name("value_like/new/1"), |b| {
        b.iter(|| parse_value_like_new(black_box("1")))
    });

    // id-expression
    c.bench_function(&name("value_like/old/x"), |b| {
        b.iter(|| parse_value_like_old(black_box("x")))
    });
    c.bench_function(&name("value_like/new/x"), |b| {
        b.iter(|| parse_value_like_new(black_box("x")))
    });

    // Parenthesized value
    c.bench_function(&name("value_like/old/parenthesized"), |b| {
        b.iter(|| parse_value_like_old(black_box("(1)")))
    });
    c.bench_function(&name("value_like/new/parenthesized"), |b| {
        b.iter(|| parse_value_like_new(black_box("(1)")))
    });

    // Unary operator
    c.bench_function(&name("value_like/old/--x"), |b| {
        b.iter(|| parse_value_like_old(black_box("--x")))
    });
    c.bench_function(&name("value_like/new/--x"), |b| {
        b.iter(|| parse_value_like_new(black_box("--x")))
    });

    // new-expression
    c.bench_function(&name("value_like/old/new_expression"), |b| {
        b.iter(|| parse_value_like_old(black_box("new T")))
    });
    c.bench_function(&name("value_like/new/new_expression"), |b| {
        b.iter(|| parse_value_like_new(black_box("new T")))
    });

    // Function call
    c.bench_function(&name("value_like/old/function_call"), |b| {
        b.iter(|| parse_value_like_old(black_box("f()")))
    });
    c.bench_function(&name("value_like/new/function_call"), |b| {
        b.iter(|| parse_value_like_new(black_box("f()")))
    });

    // Binary operator
    c.bench_function(&name("value_like/old/binary_op"), |b| {
        b.iter(|| parse_value_like_old(black_box("1+1")))
    });
    c.bench_function(&name("value_like/new/binary_op"), |b| {
        b.iter(|| parse_value_like_new(black_box("1+1")))
    });

    // Ternary operator
    c.bench_function(&name("value_like/old/ternary_op"), |b| {
        b.iter(|| parse_value_like_old(black_box("1?2:3")))
    });
    c.bench_function(&name("value_like/new/ternary_op"), |b| {
        b.iter(|| parse_value_like_new(black_box("1?2:3")))
    });

    // Ternary operator
    c.bench_function(&name("value_like/old/member"), |b| {
        b.iter(|| parse_value_like_old(black_box("a.b")))
    });
    c.bench_function(&name("value_like/new/member"), |b| {
        b.iter(|| parse_value_like_new(black_box("a.b")))
    });

    // Array indexing
    c.bench_function(&name("value_like/old/array"), |b| {
        b.iter(|| parse_value_like_old(black_box("a[1]")))
    });
    c.bench_function(&name("value_like/new/array"), |b| {
        b.iter(|| parse_value_like_new(black_box("a[1]")))
    });

    // Postfix operator
    c.bench_function(&name("value_like/old/postfix"), |b| {
        b.iter(|| parse_value_like_old(black_box("a++")))
    });
    c.bench_function(&name("value_like/new/postfix"), |b| {
        b.iter(|| parse_value_like_new(black_box("a++")))
    });

    // Not a value
    c.bench_function(&name("value_like/old/fail"), |b| {
        b.iter(|| parse_value_like_old(black_box("#")))
    });
    c.bench_function(&name("value_like/new/fail"), |b| {
        b.iter(|| parse_value_like_new(black_box("#")))
    });
}

criterion_group!(benches, values);
criterion_main!(benches);
