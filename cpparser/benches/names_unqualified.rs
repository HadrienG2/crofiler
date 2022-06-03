use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn names_unqualified(c: &mut Criterion) {
    use cpparser::names::unqualified;
    let name = |s| format!("names::unqualified::{s}");

    // Various flavors of named entity
    c.bench_function(&name("unqualified_id/a"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("a")))
    });
    c.bench_function(&name("unqualified_id/~a"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("~a")))
    });
    c.bench_function(&name("unqualified_id/a<>"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("a<>")))
    });

    // Cheap and expensive operator overload
    c.bench_function(&name("unqualified_id/operator+"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("operator+")))
    });
    c.bench_function(&name("unqualified_id/operator int"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("operator int")))
    });

    // Decltype expression
    c.bench_function(&name("unqualified_id/decltype"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("decltype(1)")))
    });

    // Anonymous entities
    c.bench_function(&name("unqualified_id/lambda"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("(lambda at x.hpp:1:2)")))
    });
    c.bench_function(&name("unqualified_id/anonymous"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("(anonymous namespace)")))
    });

    // Failure
    c.bench_function(&name("unqualified_id/fail"), |b| {
        b.iter(|| unqualified::unqualified_id(black_box("#")))
    });
}

criterion_group!(benches, names_unqualified);
criterion_main!(benches);
