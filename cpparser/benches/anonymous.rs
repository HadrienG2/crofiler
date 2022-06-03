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

    // Lambda parsing: old-style, without interning...
    c.bench_function(&name("lambda/old/pass"), |b| {
        b.iter(|| anonymous::lambda(black_box("(lambda at /x.cpp:1:2)"), std::convert::identity))
    });
    c.bench_function(&name("lambda/old/fail"), |b| {
        b.iter(|| anonymous::lambda(black_box("NotALambda"), std::convert::identity))
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
