use std::{fmt::Display, io::Read};

use clap::Parser;
use cpparser::{
    display::{CustomDisplay, DisplayState, RecursionDepths},
    EntityParser,
};

/// Simplify a C++ entity name into a human-readable form
///
/// The entity can be provided from either directly on the command line (with
/// suitable quoting and escaping) or by piping a file into standard input.
///
#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    /// Recursion depth
    #[clap(flatten)]
    recursion_depths: RecursionDepths,

    /// C++ entity name to be simplified (can also be specified via stdin)
    entity: Option<String>,
}
//
impl Display for Args {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Extract input string
        let entity = self
            .entity
            .as_ref()
            .expect("Please provide input via either CLI or stdin");

        // Parse C++ entity name
        let parser = EntityParser::new();
        let entity = parser
            .parse_entity(&entity)
            .expect("Failed to parse entity name");
        let entities = parser.finalize();
        let entity = entities.entity(entity);

        // Display it
        println!("Recursion depths are {:#?}", entity.recursion_depths());
        entity.display(f, &DisplayState::new(self.recursion_depths))
    }
}

fn main() {
    // Parse CLI arguments
    let mut args = Args::parse();

    // Get C++ entity name from either CLI or stdin
    let mut stdin = String::new();
    args.entity = args.entity.take().or_else(|| {
        if atty::isnt(atty::Stream::Stdin) {
            std::io::stdin()
                .read_to_string(&mut stdin)
                .expect("Failed to read entity from standard input");
            Some(stdin)
        } else {
            None
        }
    });

    // Display it
    println!("{args}");
}
