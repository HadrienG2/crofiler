use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn operators_overloads(c: &mut Criterion) {
    use cpparser::operators::overloads;
    let name = |s| format!("operators::overloads::{s}");

    c.bench_function(&name("operator_overload/+"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator+")))
    });
    c.bench_function(&name("operator_overload/,"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator,")))
    });
    c.bench_function(&name("operator_overload/++"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator++")))
    });
    c.bench_function(&name("operator_overload/>>"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator>>")))
    });
    c.bench_function(&name("operator_overload/+="), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator+=")))
    });
    c.bench_function(&name("operator_overload/>="), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator>=")))
    });
    c.bench_function(&name("operator_overload/<=>"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator<=>")))
    });

    c.bench_function(&name("operator_overload/()"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator()")))
    });
    c.bench_function(&name("operator_overload/[]"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator[]")))
    });

    c.bench_function(&name("operator_overload/\"\" _s"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator\"\" _s")))
    });

    c.bench_function(&name("operator_overload/new"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator new")))
    });
    c.bench_function(&name("operator_overload/new[]"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator new[]")))
    });

    c.bench_function(&name("operator_overload/delete"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator delete")))
    });
    c.bench_function(&name("operator_overload/delete[]"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator delete[]")))
    });

    c.bench_function(&name("operator_overload/co_await"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator co_await")))
    });

    c.bench_function(&name("operator_overload/int"), |b| {
        b.iter(|| overloads::operator_overload(black_box("operator int")))
    });

    c.bench_function(&name("operator_overload/fail"), |b| {
        b.iter(|| overloads::operator_overload(black_box("fail")))
    });
}

criterion_group!(benches, operators_overloads);
criterion_main!(benches);
