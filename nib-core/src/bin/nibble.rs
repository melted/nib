use std::io::{self, Read};

use nibble::core;
use nibble::parser;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buffer)?;
    parser::dump_lex(&buffer)?;
    parser::dump_prog(&buffer)?;
    let modul = parser::parse_declarations(None, &buffer)?;
    let desugared = core::desugar(modul)?;
    println!("{}", desugared);
    Ok(())
}
