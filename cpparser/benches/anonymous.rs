use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn anonymous(c: &mut Criterion) {
    // Failure performance is not a concern here as this token is super rare and
    // only tried when the input doesn't match the type grammar
    c.bench_function("parse_unknown_entity", |b| {
        b.iter(|| EntityParser::parse_unknown_entity(black_box("<unknown>")))
    });

    // Lambda parsing
    let parser = EntityParser::new();
    c.bench_function("parse_lambda/pass", |b| {
        b.iter(|| parser.parse_lambda(black_box("(lambda at /x.cpp:1:2)")))
    });
    c.bench_function("parse_lambda/fail", |b| {
        b.iter(|| parser.parse_lambda(black_box("NotALambda")))
    });

    // Anonymous entity parsing
    c.bench_function("parse_anonymous/empty", |b| {
        b.iter(|| parser.parse_anonymous(black_box("(anonymous)")))
    });
    c.bench_function("parse_anonymous/namespace", |b| {
        b.iter(|| parser.parse_anonymous(black_box("(anonymous namespace)")))
    });
    c.bench_function("parse_anonymous/fail", |b| {
        b.iter(|| parser.parse_anonymous(black_box("NotAnonymous")))
    });
}

criterion_group!(benches, anonymous);
criterion_main!(benches);
