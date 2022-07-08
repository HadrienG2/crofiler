use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn types_specifiers_legacy(c: &mut Criterion) {
    let mut parser = EntityParser::new();

    // Most common legacy type name (4.3M occurences in test dataset)
    c.bench_function("parse_legacy_name/Double", |b| {
        b.iter(|| parser.parse_legacy_name(black_box("double")))
    });

    // Second most common (1.3M occurences in test dataset)
    c.bench_function("parse_legacy_name/UnsignedLongLong", |b| {
        b.iter(|| parser.parse_legacy_name(black_box("unsigned long long")))
    });

    // All other type names are much less frequent (<100K occurences), but
    // parser failure speed is also very important
    c.bench_function("parse_legacy_name/fail/early", |b| {
        b.iter(|| parser.parse_legacy_name(black_box("x")))
    });
    c.bench_function("parse_legacy_name/fail/late", |b| {
        b.iter(|| parser.parse_legacy_name(black_box("char_traits")))
    });
}

criterion_group!(benches, types_specifiers_legacy);
criterion_main!(benches);
