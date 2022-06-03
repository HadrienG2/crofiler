use cpparser::EntityParser;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn names_atoms(c: &mut Criterion) {
    use cpparser::names::atoms;
    let name = |s| format!("names::atoms::{s}");

    // Identifier parsing: without caching...
    c.bench_function(&name("identifier/cold/small"), |b| {
        b.iter(|| atoms::identifier(black_box("a")))
    });
    c.bench_function(&name("identifier/cold/large"), |b| {
        b.iter(|| atoms::identifier(black_box("ItShouldntGetLongerThanThis")))
    });
    c.bench_function(&name("identifier/cold/fail"), |b| {
        b.iter(|| atoms::identifier(black_box("6")))
    });

    // ...and with caching (TODO: Also measure cold cache perf & retrieval perf)
    let parser = EntityParser::new();
    c.bench_function(&name("identifier/hot/small"), |b| {
        b.iter(|| parser.parse_identifier(black_box("a")))
    });
    c.bench_function(&name("identifier/hot/large"), |b| {
        b.iter(|| parser.parse_identifier(black_box("ItShouldntGetLongerThanThis")))
    });
    c.bench_function(&name("identifier/hot/fail"), |b| {
        b.iter(|| parser.parse_identifier(black_box("6")))
    });

    // Shortest keyword we parse
    let new = atoms::keyword("new");
    c.bench_function(&name("keyword/new/pass"), |b| {
        b.iter(|| new(black_box("new")))
    });
    c.bench_function(&name("keyword/new/fail/early"), |b| {
        b.iter(|| new(black_box("typename")))
    });
    c.bench_function(&name("keyword/new/fail/late"), |b| {
        b.iter(|| new(black_box("newer")))
    });

    // Longest keyword we parse
    let typename = atoms::keyword("typename");
    c.bench_function(&name("keyword/typename/pass"), |b| {
        b.iter(|| typename(black_box("typename")))
    });
    c.bench_function(&name("keyword/typename/fail/early"), |b| {
        b.iter(|| typename(black_box("new")))
    });
    c.bench_function(&name("keyword/typename/fail/late"), |b| {
        b.iter(|| typename(black_box("typenamed")))
    });

    // Keyword set
    let keywords = atoms::keywords([
        "int", "char", "double", "unsigned", "short", "long", "signed",
    ]);
    c.bench_function(&name("keywords/pass/first"), |b| {
        b.iter(|| keywords(black_box("int")))
    });
    c.bench_function(&name("keywords/pass/last"), |b| {
        b.iter(|| keywords(black_box("signed")))
    });
    c.bench_function(&name("keywords/fail"), |b| {
        b.iter(|| keywords(black_box("x")))
    });
}

criterion_group!(benches, names_atoms);
criterion_main!(benches);
