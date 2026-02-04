// The trait implemented by the interpreters.
// This is not exhaustive, just the minimal set so that the nibble binary can
// use the same logic for both interpreters.
// Since having two interpreters is a temporary situation (hopefully), there
// is no need to expand this to cover more. It would be awfully tricky once
// you'd need to be generic over the two kinds of values.

use crate::common::Result;
use std::path::Path;

pub trait Interpreter {
    fn load(&mut self, path: &Path, reload: bool) -> Result<()>;
    fn add_code(&mut self, name: &str, code: &str) -> Result<()>;
    fn set_output_core(&mut self, output: bool);
}
