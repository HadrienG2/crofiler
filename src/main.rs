mod trace;

use trace::ClangTrace;

fn main() {
    let trace =
        ClangTrace::from_file("2020-05-25_CombinatorialKalmanFilterTests.cpp.json").unwrap();
    println!("Global statistics: {:#?}", trace.global_stats());
}
