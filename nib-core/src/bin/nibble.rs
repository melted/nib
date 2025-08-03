use std::env::args;
use std::io::{self, stderr, stdin, Read, Write};
use std::process::exit;

use nibble::common::Error;
use nibble::runtime::Runtime;

/// Simple runner of Nib code. Anything more elaborate goes into
/// another crate, where it can pull in dependencies and go wild
/// in general.
fn main() -> io::Result<()> {
    let opts = parse_options();
    let mut rt = Runtime::new();
    let prelude_code = include_str!("../../lib/prelude.nib");
    if !opts.no_prelude {
        rt.add_code("prelude", prelude_code)?;
    }
    let res = if opts.files.is_empty() {
        let mut buffer = String::new();
        let read = stdin().read_to_string(&mut buffer)?;
        rt.add_code("stdin", &buffer)
    } else {
        let mut res = Ok(());
        for f in opts.files {
            res = rt.load(&f);
            if res.is_err() {
                break;
            }
        }
        res
    };
    if let Err(err) = res {
        match err {
            Error::NibExit{ exit_code } => {
                exit(exit_code);
            }
            _ => {
                stderr().write(&format!("{}", err).as_bytes())?;
            }
        }
    }
    Ok(())
}

pub struct Options {
    pub no_prelude : bool,
    pub files : Vec<String>
}

impl Options {
    fn new() -> Self {
        Options { no_prelude: false, files: Vec::new() }
    }
}

fn parse_options() -> Options {
    let mut opts = Options::new();
    for arg in args().skip(1) {
        match arg {
            _ if arg == "--no-prelude" => {
                opts.no_prelude = true;
            },
            file => {
                opts.files.push(file);
            }
        }
    }
    opts
}