use std::env::args;
use std::io::{self, Read, stdin};

use nibble::runtime::Runtime;

/// Simple runner of Nib code. Anything more elaborate goes into
/// another crate, where it can pull in dependencies and go wild
/// in general.
fn main() -> io::Result<()> {
    env_logger::init();
    let args: Vec<_> = args().collect();
    let mut rt = Runtime::new();
    if args.len() < 2 {
        let mut buffer = String::new();
        let read = stdin().read_to_string(&mut buffer)?;
        rt.add_code("stdin", &buffer)?;
    } else {
        rt.load(&args[1])?;
    }
    Ok(())
}
