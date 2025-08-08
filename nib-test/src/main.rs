// Test runner for Nib tests, this reads metadata from comments in the testcase and
// runs it. If output is missing it will add that to the end of the testcase,
// if it is present it will compare the actual output and that specified in
// the testcase file.

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
/// Test runner for Nib tests.
#[command(version, about, long_about = None)]
struct Options {
    /// Update output for tests that are failing
    #[arg(long)]
    update:bool,

    /// Tests to run
    tests:Vec<PathBuf>
}

fn main() {
    let cli = Options::parse();
    println!("Hello, world!");
}
