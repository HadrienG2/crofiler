use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn operators_overloads(c: &mut Criterion) {
    use cpparser::operators::overloads;
    let name = |s| format!("operators::overloads::{s}");

    let parse_operator_overload =
        |s| overloads::operator_overload(s, &atoms::identifier, &Path::new);
    let parser = EntityParser::new();
    c.bench_function(&name("operator_overload/old/+"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator+")))
    });
    c.bench_function(&name("operator_overload/old/,"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator,")))
    });
    c.bench_function(&name("operator_overload/old/++"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator++")))
    });
    c.bench_function(&name("operator_overload/old/>>"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator>>")))
    });
    c.bench_function(&name("operator_overload/old/+="), |b| {
        b.iter(|| parse_operator_overload(black_box("operator+=")))
    });
    c.bench_function(&name("operator_overload/old/>="), |b| {
        b.iter(|| parse_operator_overload(black_box("operator>=")))
    });
    c.bench_function(&name("operator_overload/old/<=>"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator<=>")))
    });
    c.bench_function(&name("operator_overload/new/+"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator+")))
    });
    c.bench_function(&name("operator_overload/new/,"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator,")))
    });
    c.bench_function(&name("operator_overload/new/++"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator++")))
    });
    c.bench_function(&name("operator_overload/new/>>"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator>>")))
    });
    c.bench_function(&name("operator_overload/new/+="), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator+=")))
    });
    c.bench_function(&name("operator_overload/new/>="), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator>=")))
    });
    c.bench_function(&name("operator_overload/new/<=>"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator<=>")))
    });
    //
    c.bench_function(&name("operator_overload/old/()"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator()")))
    });
    c.bench_function(&name("operator_overload/old/[]"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator[]")))
    });
    c.bench_function(&name("operator_overload/new/()"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator()")))
    });
    c.bench_function(&name("operator_overload/new/[]"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator[]")))
    });
    //
    c.bench_function(&name("operator_overload/old/\"\" _s"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator\"\" _s")))
    });
    c.bench_function(&name("operator_overload/new/\"\" _s"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator\"\" _s")))
    });
    //
    c.bench_function(&name("operator_overload/old/new"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator new")))
    });
    c.bench_function(&name("operator_overload/old/new[]"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator new[]")))
    });
    c.bench_function(&name("operator_overload/new/new"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator new")))
    });
    c.bench_function(&name("operator_overload/new/new[]"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator new[]")))
    });
    //
    c.bench_function(&name("operator_overload/old/delete"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator delete")))
    });
    c.bench_function(&name("operator_overload/old/delete[]"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator delete[]")))
    });
    c.bench_function(&name("operator_overload/new/delete"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator delete")))
    });
    c.bench_function(&name("operator_overload/new/delete[]"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator delete[]")))
    });
    //
    c.bench_function(&name("operator_overload/old/co_await"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator co_await")))
    });
    c.bench_function(&name("operator_overload/new/co_await"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator co_await")))
    });
    //
    c.bench_function(&name("operator_overload/old/int"), |b| {
        b.iter(|| parse_operator_overload(black_box("operator int")))
    });
    c.bench_function(&name("operator_overload/new/int"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("operator int")))
    });
    //
    c.bench_function(&name("operator_overload/old/fail"), |b| {
        b.iter(|| parse_operator_overload(black_box("fail")))
    });
    c.bench_function(&name("operator_overload/new/fail"), |b| {
        b.iter(|| parser.parse_operator_overload(black_box("fail")))
    });
}

criterion_group!(benches, operators_overloads);
criterion_main!(benches);
