use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn anonymous(c: &mut Criterion) {
    use cpparser::anonymous;
    let name = |s| format!("anonymous::{s}");

    // Failure performance is not a concern here as this token is super rare and
    // only tried when the input doesn't match the type grammar
    c.bench_function(&name("unknown_entity"), |b| {
        b.iter(|| EntityParser::parse_unknown_entity(black_box("<unknown>")))
    });

    c.bench_function(&name("lambda/pass"), |b| {
        b.iter(|| anonymous::lambda(black_box("(lambda at x.cpp:1:2)")))
    });
    c.bench_function(&name("lambda/fail"), |b| {
        b.iter(|| anonymous::lambda(black_box("NotALambda")))
    });

    c.bench_function(&name("anonymous/empty"), |b| {
        b.iter(|| anonymous::anonymous(black_box("(anonymous)")))
    });
    c.bench_function(&name("anonymous/namespace"), |b| {
        b.iter(|| anonymous::anonymous(black_box("(anonymous namespace)")))
    });
    c.bench_function(&name("anonymous/fail"), |b| {
        b.iter(|| anonymous::anonymous(black_box("NotAnonymous")))
    });
}

criterion_group!(benches, anonymous);
criterion_main!(benches);
