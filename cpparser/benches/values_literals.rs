use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn values_literals(c: &mut Criterion) {
    use cpparser::values::literals;
    let name = |s| format!("values::literals::{s}");

    c.bench_function(&name("literal/1"), |b| {
        b.iter(|| literals::literal(black_box("1")))
    });
    c.bench_function(&name("literal/1U"), |b| {
        b.iter(|| literals::literal(black_box("1U")))
    });
    c.bench_function(&name("literal/1ULL"), |b| {
        b.iter(|| literals::literal(black_box("1ULL")))
    });
    c.bench_function(&name("literal/1_x"), |b| {
        b.iter(|| literals::literal(black_box("1_x")))
    });

    c.bench_function(&name("literal/'x'"), |b| {
        b.iter(|| literals::literal(black_box("1")))
    });
    c.bench_function(&name("literal/'\\n'"), |b| {
        b.iter(|| literals::literal(black_box("'\\n'")))
    });

    c.bench_function(&name("literal/fail"), |b| {
        b.iter(|| literals::literal(black_box("fail")))
    });
}

criterion_group!(benches, values_literals);
criterion_main!(benches);
