mod trace;

use trace::ClangTrace;

fn main() {
    let trace =
        ClangTrace::from_file("2020-05-25_CombinatorialKalmanFilterTests.cpp.json").unwrap();
    println!("Profile from {}", trace.process_name());
    println!("Global statistics: {:#?}", trace.global_stats());
    println!("Tree roots:");
    for root in trace.hierarchy_iter() {
        println!("- {:#?}", root);
    }
}
