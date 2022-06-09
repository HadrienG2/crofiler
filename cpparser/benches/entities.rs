use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn entities(c: &mut Criterion) {
    let parser = EntityParser::new();
    c.bench_function("parse_entity/T", |b| {
        b.iter(|| parser.parse_entity(black_box("T")))
    });
    c.bench_function("entity/unknown", |b| {
        b.iter(|| parser.parse_entity(black_box("<unknown>")))
    });
}

criterion_group!(benches, entities);
criterion_main!(benches);
