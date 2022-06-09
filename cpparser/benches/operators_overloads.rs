use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn operators_overloads(c: &mut Criterion) {
    let parser = EntityParser::new();

    // Arithmetic and comparison
    c.bench_function("parse_operator_overload/+", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator+")))
    });
    c.bench_function("parse_operator_overload/,", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator,")))
    });
    c.bench_function("parse_operator_overload/++", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator++")))
    });
    c.bench_function("parse_operator_overload/>>", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator>>")))
    });
    c.bench_function("parse_operator_overload/+=", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator+=")))
    });
    c.bench_function("parse_operator_overload/>=", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator>=")))
    });
    c.bench_function("parse_operator_overload/<=>", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator<=>")))
    });

    // Function call and array indexing
    c.bench_function("parse_operator_overload/()", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator()")))
    });
    c.bench_function("parse_operator_overload/[]", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator[]")))
    });

    // Custom literal
    c.bench_function("parse_operator_overload/\"\" _s", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator\"\" _s")))
    });

    // Allocation
    c.bench_function("parse_operator_overload/new", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator new")))
    });
    c.bench_function("parse_operator_overload/new[]", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator new[]")))
    });

    // Deallocation
    c.bench_function("parse_operator_overload/delete", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator delete")))
    });
    c.bench_function("parse_operator_overload/delete[]", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator delete[]")))
    });

    // Coroutine co_await
    c.bench_function("parse_operator_overload/co_await", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator co_await")))
    });

    // Type conversion
    c.bench_function("parse_operator_overload/int", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator int")))
    });

    // Parse failure
    c.bench_function("parse_operator_overload/fail", |b| {
        b.iter(|| parser.parse_operator_overload(black_box("fail")))
    });
}

criterion_group!(benches, operators_overloads);
criterion_main!(benches);
