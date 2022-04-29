mod trace;

use trace::TimeTrace;

fn main() {
    let trace = TimeTrace::from_file("2020-05-25_CombinatorialKalmanFilterTests.cpp.json").unwrap();
    println!("Global statistics: {:#?}", trace.global_stats());
}
