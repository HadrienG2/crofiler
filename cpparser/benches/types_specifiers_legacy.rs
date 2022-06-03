use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn types_specifiers_legacy(c: &mut Criterion) {
    use cpparser::types::specifiers::legacy;
    let name = |s| format!("types::specifiers::legacy::{s}");

    // Most common legacy type name (4.3M occurences in test dataset)
    let parser = EntityParser::new();
    c.bench_function(&name("legacy_name/old/Double"), |b| {
        b.iter(|| legacy::legacy_name(black_box("double")))
    });
    c.bench_function(&name("legacy_name/new/Double"), |b| {
        b.iter(|| parser.parse_legacy_name(black_box("double")))
    });

    // Second most common (1.3M occurences in test dataset)
    c.bench_function(&name("legacy_name/old/UnsignedLongLong"), |b| {
        b.iter(|| legacy::legacy_name(black_box("unsigned long long")))
    });
    c.bench_function(&name("legacy_name/new/UnsignedLongLong"), |b| {
        b.iter(|| parser.parse_legacy_name(black_box("unsigned long long")))
    });

    // All other type names are much less frequent (<100K occurences), but
    // parser failure speed is also very important
    c.bench_function(&name("legacy_name/old/fail/early"), |b| {
        b.iter(|| legacy::legacy_name(black_box("x")))
    });
    c.bench_function(&name("legacy_name/new/fail/early"), |b| {
        b.iter(|| parser.parse_legacy_name(black_box("x")))
    });
    c.bench_function(&name("legacy_name/old/fail/late"), |b| {
        b.iter(|| legacy::legacy_name(black_box("char_traits")))
    });
    c.bench_function(&name("legacy_name/new/fail/late"), |b| {
        b.iter(|| parser.parse_legacy_name(black_box("char_traits")))
    });
}

criterion_group!(benches, types_specifiers_legacy);
criterion_main!(benches);
