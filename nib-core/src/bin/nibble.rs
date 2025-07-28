use std::env::args;
use std::io::{self, stdin, Read};

use nibble::core;
use nibble::parser;
use nibble::runtime::Runtime;

fn main() -> io::Result<()> {
    let args : Vec<_> = args().collect();
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
