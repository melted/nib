use std::io::{self, Read};

use nibble::parser;



fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buffer)?;
    parser::dump_lex(&buffer)?;
    parser::dump_prog(&buffer)?;
    Ok(())
}