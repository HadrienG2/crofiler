use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn anonymous(c: &mut Criterion) {
    use cpparser::anonymous;
    let name = |s| format!("anonymous::{s}");

    // Failure performance is not a concern here as this token is super rare and
    // only tried when the input doesn't match the type grammar
    c.bench_function(&name("unknown_entity"), |b| {
        b.iter(|| EntityParser::parse_unknown_entity(black_box("<unknown>")))
    });

    // Lambda parsing: old-style, without interning...
    let parse_lambda = |s| anonymous::lambda(s, &Path::new);
    c.bench_function(&name("lambda/old/pass"), |b| {
        b.iter(|| parse_lambda(black_box("(lambda at /x.cpp:1:2)")))
    });
    c.bench_function(&name("lambda/old/fail"), |b| {
        b.iter(|| parse_lambda(black_box("NotALambda")))
    });

    // ...and new-style with interning
    // TODO: Also measure cold cache perf & retrieval perf
    let parser = EntityParser::new();
    c.bench_function(&name("lambda/new/pass"), |b| {
        b.iter(|| parser.parse_lambda(black_box("(lambda at /x.cpp:1:2)")))
    });
    c.bench_function(&name("lambda/new/fail"), |b| {
        b.iter(|| parser.parse_lambda(black_box("NotALambda")))
    });

    // Anonymous entity parsing: old-style, without interning...
    let parse_anonymous = |s| anonymous::anonymous(s, &atoms::identifier);
    c.bench_function(&name("anonymous/old/empty"), |b| {
        b.iter(|| parse_anonymous(black_box("(anonymous)")))
    });
    c.bench_function(&name("anonymous/old/namespace"), |b| {
        b.iter(|| parse_anonymous(black_box("(anonymous namespace)")))
    });
    c.bench_function(&name("anonymous/old/fail"), |b| {
        b.iter(|| parse_anonymous(black_box("NotAnonymous")))
    });

    // ...and new-style with interning
    // TODO: Also measure cold cache perf & retrieval perf
    let parser = EntityParser::new();
    c.bench_function(&name("anonymous/new/empty"), |b| {
        b.iter(|| parser.parse_anonymous(black_box("(anonymous)")))
    });
    c.bench_function(&name("anonymous/new/namespace"), |b| {
        b.iter(|| parser.parse_anonymous(black_box("(anonymous namespace)")))
    });
    c.bench_function(&name("anonymous/new/fail"), |b| {
        b.iter(|| parser.parse_anonymous(black_box("NotAnonymous")))
    });
}

criterion_group!(benches, anonymous);
criterion_main!(benches);
