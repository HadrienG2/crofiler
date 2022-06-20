use clap::Parser;
use cpparser::EntityParser;

/// Simplify a C++ entity name into a human-readable form
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// C++ entity name to be simplified
    // TODO: Also accept it from stdin
    entity: String,
}

fn main() {
    let args = Args::parse();

    let parser = EntityParser::new();
    let entity = parser
        .parse_entity(&args.entity)
        .expect("Failed to parse entity name");
    let entities = parser.finalize();

    println!("{}", entities.entity(entity));
}
