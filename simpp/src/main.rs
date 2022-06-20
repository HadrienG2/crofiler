use std::io::Read;

use clap::Parser;
use cpparser::{display::CustomDisplay, EntityParser};

/// Simplify a C++ entity name into a human-readable form
///
/// The entity can be provided from either directly on the command line (with
/// suitable quoting and escaping) or by piping a file into standard input.
///
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// C++ entity name to be simplified (can also be specified via stdin)
    entity: Option<String>,
}

fn main() {
    // Parse CLI arguments
    let args = Args::parse();

    // Get C++ entity name from either CLI or stdin
    let mut stdin = String::new();
    let entity = args
        .entity
        .as_ref()
        .or_else(|| {
            if atty::isnt(atty::Stream::Stdin) {
                std::io::stdin()
                    .read_to_string(&mut stdin)
                    .expect("Failed to read entity from standard input");
                Some(&stdin)
            } else {
                None
            }
        })
        .expect("No C++ entity specified via CLI or standard input");

    // Parse C++ entity name
    let parser = EntityParser::new();
    let entity = parser
        .parse_entity(&entity)
        .expect("Failed to parse entity name");
    let entities = parser.finalize();
    let entity = entities.entity(entity);

    // Display it
    println!("{}", entity);
    println!("Recursion depths are {:#?}", entity.recursion_depths());
}
