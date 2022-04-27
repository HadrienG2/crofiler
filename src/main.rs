mod parser;

use crate::parser::{DisplayTimeUnit, TraceDataObject, TraceEvent};
use serde_json as json;
use std::{collections::HashMap, fs::File, io::Read};

fn main() {
    const FILENAME: &str = "2020-05-25_CombinatorialKalmanFilterTests.cpp.json";
    let mut profile_str = String::new();
    File::open(FILENAME)
        .unwrap()
        .read_to_string(&mut profile_str)
        .unwrap();
    let profile_ctf = json::from_str::<TraceDataObject>(&profile_str).unwrap();

    assert_eq!(profile_ctf.displayTimeUnit, DisplayTimeUnit::ms);
    assert_eq!(profile_ctf.systemTraceEvents, None);
    assert_eq!(profile_ctf.powerTraceAsString, None);
    assert_eq!(profile_ctf.stackFrames, None);
    assert_eq!(profile_ctf.samples, None);
    assert_eq!(profile_ctf.controllerTraceDataKey, None);
    assert_eq!(profile_ctf.extra, HashMap::new());

    let mut display_period = 1;
    let display_period_increment = 10;
    let display_increment_period = 10;
    let mut displayed_events = 0;
    let mut current_event = 0;
    for e in profile_ctf.traceEvents.into_iter() {
        match e {
            TraceEvent::M(_) => println!("{:#?}", e),
            TraceEvent::X { .. } => {
                if current_event % display_period == 0 {
                    if (displayed_events > 0) && (display_period > 1) {
                        println!("... skipped {display_period} complete events ...");
                    }
                    println!("{:#?}", e);
                    displayed_events += 1;
                    if displayed_events % display_increment_period == 0 {
                        display_period *= display_period_increment;
                        println!(
                            "... will now only display 1 in {display_period} complete events ..."
                        );
                    }
                }
                current_event += 1;
            }
            _ => panic!("Unexpected event type"),
        }
    }
}
