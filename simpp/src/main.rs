use std::{fmt::Display, io::Read};

use clap::Parser;
use cpparser::{
    display::{CustomDisplay, DisplayState},
    EntityParser, EntityView,
};
use std::fmt::Write;
use unicode_width::UnicodeWidthStr;

/// Simplify a C++ entity name into a human-readable form
///
/// The entity can be provided from either directly on the command line (with
/// suitable quoting and escaping) or by piping a file into standard input.
///
#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    /// Terminal column budget
    #[clap(short, long)]
    cols: usize,

    /// C++ entity name to be simplified (can also be specified via stdin)
    entity: Option<String>,
}

/// Helper to display a C++ entity with a custom configuration
struct EntityDisplay<'entities> {
    /// C++ entity to be displayed
    entity: &'entities EntityView<'entities>,

    /// Initial display state
    state: DisplayState,
}
//
impl Display for EntityDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.entity.display(f, &self.state)
    }
}

fn main() {
    // Parse CLI arguments
    let mut args = Args::parse();

    // Get C++ entity name from either CLI or stdin
    let mut stdin = String::new();
    let entity = args
        .entity
        .take()
        .or_else(|| {
            if atty::isnt(atty::Stream::Stdin) {
                std::io::stdin()
                    .read_to_string(&mut stdin)
                    .expect("Failed to read entity from standard input");
                Some(stdin)
            } else {
                None
            }
        })
        .expect("Please provide a C++ entity name via either CLI or stdin");

    // Parse C++ entity name
    let parser = EntityParser::new();
    let entity = parser
        .parse_entity(&entity)
        .expect("Failed to parse entity name");
    let entities = parser.finalize();
    let entity = entities.entity(entity);

    // Display it
    let mut prev_display = "…".to_string();
    let mut curr_display = String::new();
    for recursion_depth in 0..entity.recursion_depth() {
        write!(
            &mut curr_display,
            "{}",
            EntityDisplay {
                entity: &entity,
                state: DisplayState::new(recursion_depth)
            }
        )
        .unwrap();
        if curr_display.width() > args.cols {
            break;
        } else {
            prev_display.clear();
            prev_display.clone_from(&curr_display);
            curr_display.clear()
        }
    }
    println!("{prev_display}");
}
