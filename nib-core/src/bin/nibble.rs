use std::env::args;
use std::fs::read_to_string;
use std::io::{self, Read, Write, stderr, stdin};
use std::process::exit;

use nib_core::common::Error;
use nib_core::parser::dump_lex;
use nib_core::treewalker::Runtime;

/// Simple runner of Nib code. Anything more elaborate goes into
/// another crate, where it can pull in dependencies and go wild
/// in general.
fn main() -> io::Result<()> {
    let opts = parse_options();
    let mut rt = Runtime::new();
    let prelude_code = include_str!("../../lib/prelude.nib");
    let level = if opts.verbose {
        log::Level::Info
    } else {
        log::Level::Error
    };
    simple_logger::init_with_level(level).unwrap();
    if opts.output_core {
        rt.set_output_core(true);
    }
    if opts.dump_tokens {
        for f in opts.files {
            println!("dumping {}", &f);
            let code = read_to_string(f)?;
            dump_lex(&code)?;
        }
        return Ok(());
    }
    if !opts.no_prelude {
        rt.add_code("prelude", prelude_code)?;
    }
    let res = if opts.files.is_empty() {
        let mut buffer = String::new();
        let _read = stdin().read_to_string(&mut buffer)?;
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
            Error::NibExit { exit_code } => {
                exit(exit_code);
            }
            _ => {
                stderr().write_all(format!("{}\n", err).as_bytes())?;
                exit(1);
            }
        }
    }
    Ok(())
}

pub struct Options {
    pub no_prelude: bool,
    pub verbose: bool,
    pub use_treewalker: bool,
    pub output_core: bool,
    pub dump_tokens: bool,
    pub files: Vec<String>,
}

impl Options {
    fn new() -> Self {
        Options {
            no_prelude: false,
            verbose: false,
            use_treewalker: true,
            output_core: false,
            dump_tokens: false,
            files: Vec::new(),
        }
    }
}

fn parse_options() -> Options {
    let mut opts = Options::new();
    for arg in args().skip(1) {
        match arg {
            _ if arg == "--no-prelude" => {
                opts.no_prelude = true;
            }
            _ if arg == "--verbose" => {
                opts.verbose = true;
            }
            _ if arg == "--output-core" => {
                opts.output_core = true;
            }
            _ if arg == "--dump-tokens" => {
                opts.dump_tokens = true;
            }
            file => {
                opts.files.push(file);
            }
        }
    }
    opts
}
