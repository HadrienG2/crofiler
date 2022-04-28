mod trace;

use trace::TimeTrace;

fn main() {
    let trace = TimeTrace::load("2020-05-25_CombinatorialKalmanFilterTests.cpp.json").unwrap();
}
