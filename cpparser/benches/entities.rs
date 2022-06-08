use cpparser::{names::atoms, EntityParser};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::path::Path;

fn entities(c: &mut Criterion) {
    let name = |s| format!("crate::{s}");

    let parser = EntityParser::new();
    let parse_entity = |s| cpparser::entity(s, &atoms::identifier, &Path::new);

    c.bench_function(&name("entity/old/T"), |b| {
        b.iter(|| parse_entity(black_box("T")))
    });
    c.bench_function(&name("entity/new/T"), |b| {
        b.iter(|| parser.parse_entity(black_box("T")))
    });

    c.bench_function(&name("entity/old/unknown"), |b| {
        b.iter(|| parse_entity(black_box("<unknown>")))
    });
    c.bench_function(&name("entity/new/unknown"), |b| {
        b.iter(|| parser.parse_entity(black_box("<unknown>")))
    });
}

criterion_group!(benches, entities);
criterion_main!(benches);
