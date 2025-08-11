// Test runner for Nib tests, this reads metadata from comments in the testcase and
// runs it. If output is missing it will add that to the end of the testcase,
// if it is present it will compare the actual output and that specified in
// the testcase file.

use std::{
    collections::HashSet,
    env::{self, consts::EXE_SUFFIX},
    fs::{read_dir, File},
    io::{self, Write},
    path::{Path, PathBuf},
    process::{Command, ExitCode},
};

use clap::Parser;

#[derive(Parser, Debug)]
/// Test runner for Nib tests.
#[command(version, about, long_about = None)]
struct Options {
    /// Update output for tests that are failing
    #[arg(long)]
    update: bool,

    /// The nibble executable to use
    #[arg(long)]
    nibble_path: Option<PathBuf>,

    // Tags to run
    #[arg(long)]
    tags: Vec<String>,

    /// Tests to run
    tests: Vec<PathBuf>,
}

fn main() -> ExitCode {
    let opts = Options::parse();
    
    match run_tests(&opts) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            println!("{}", err);
            ExitCode::FAILURE
        }
    }
}

fn run_tests(opts: &Options) -> io::Result<()> {
    let nib_path = get_nibble_path(opts)?;
    let mut count = 0;
    let mut tests = Vec::new();
    for t in &opts.tests {
        if t.is_dir() {
            get_tests(t, &mut tests)?;
        } else {
            tests.push(t.clone());
        }
    }

    let total = tests.len();
    for t in &tests {
        let res = run_test(opts, &nib_path, t)?;
        if !res {
            count += 1;
        }
    }
    println!("{}/{} test(s) passed", total - count, total);
    if count > 0 {
        return Err(io::Error::other(
            format!("{} test(s) failed", count),
        ));
    }
    Ok(())
}

fn get_tests(path: &PathBuf, tests: &mut Vec<PathBuf>) -> io::Result<()> {
    if path.is_dir() {
        let d = read_dir(path);
        if let Ok(iter) = d {
            for entry in iter.filter_map(|d| d.ok()) {
                get_tests(&entry.path(), tests)?;
            }
        }
    } else if path.extension().is_some_and(|os| os == "nib") {
        tests.push(path.clone());
    }
    Ok(())
}

fn run_test(opts: &Options, nib_path: &PathBuf, test: &PathBuf) -> io::Result<bool> {
    let test_code = io::read_to_string(File::open(test)?)?;
    let meta = extract_metadata(test, &test_code);
    if meta.tags.contains("disabled") {
        println!("{} ... Skipping(disabled)", &meta.name);
        return Ok(true);
    }
    let mut command = Command::new(nib_path);
    command.arg(test.as_os_str());
    if meta.tags.contains("no_prelude") {
        command.arg("--no-prelude");
    }
    let out = command.output()?;
    let out_str = str::from_utf8(&out.stdout).map_err(io::Error::other)?;
    let error_str = str::from_utf8(&out.stderr).map_err(io::Error::other)?;
    let status_code = out.status.code();
    if !out_str.is_empty() && meta.expected_output.is_empty() {
        println!("{} has no expected output. Updating.", &meta.name);
        update_test(test, &test_code, out_str, error_str, status_code)?;
    } else {
        let res = compare_output(&meta.name, "stdout", &meta.expected_output, out_str);
        let res_error = compare_output(&meta.name, "stderr", &meta.expected_error_out, error_str);
        let res_status = status_code == meta.expected_exit_code;
        if !res_status {
            println!(
                "{}: Expected exit code: {} Got: {}",
                &meta.name,
                meta.expected_exit_code.unwrap_or_default(),
                status_code.unwrap_or_default()
            );
        }
        if res && res_error && res_status {
            println!("{} ... OK", &meta.name);
        } else if opts.update {
            println!("Updating {}", &meta.name);
            update_test(test, &test_code, out_str, error_str, status_code)?;
        } else {
            println!("{} ... Failed", &meta.name);
            return Ok(false);
        }
    }
    Ok(true)
}

fn compare_output(name: &str, feed: &str, expected: &[String], output: &str) -> bool {
    let mut result = true;
    let empty = String::new();
    let line = 0;
    let mut exp_iter = expected.iter();
    let mut out_iter = output.lines();
    loop {
        let e = exp_iter.next();
        let l = out_iter.next();
        if e.is_none() && l.is_none() {
            break;
        }
        let exp_line = e.unwrap_or(&empty);
        let out_line = l.unwrap_or(&empty);
        if exp_line != out_line {
            if result {
                println!("{} of \"{}\" differs from expected", feed, name);
            }
            result = false;
            println!("Line {}: Expected: {} Got: {}", line, exp_line, out_line);
        }
    }
    result
}

fn get_nibble_path(opts: &Options) -> io::Result<PathBuf> {
    if let Some(path) = &opts.nibble_path {
        Ok(path.clone())
    } else {
        let exe_path = env::current_exe()?;
        Ok(exe_path.with_file_name("nibble".to_owned() + EXE_SUFFIX))
    }
}

#[derive(Debug, Clone)]
struct Metadata {
    name: String,
    file: PathBuf,
    tags: HashSet<String>,
    expected_output: Vec<String>,
    expected_error_out: Vec<String>,
    expected_exit_code: Option<i32>,
}

impl Metadata {
    fn new() -> Self {
        Metadata {
            name: String::new(),
            file: PathBuf::new(),
            tags: HashSet::new(),
            expected_output: Vec::new(),
            expected_error_out: Vec::new(),
            expected_exit_code: None,
        }
    }
}

fn extract_metadata(file: &Path, input: &str) -> Metadata {
    let mut meta = Metadata::new();
    meta.file = file.to_path_buf();
    let comments: Vec<_> = input
        .lines()
        .filter_map(|l| l.strip_prefix("// "))
        .collect();
    let mut in_output = false;
    let mut collected_out = Vec::new();
    for c in comments {
        if in_output {
            if c.trim_start().starts_with("@EndOutput") {
                in_output = false;
                meta.expected_output.append(&mut collected_out);
            } else if c.trim_start().starts_with("@EndErrorOut") {
                in_output = false;
                meta.expected_error_out.append(&mut collected_out);
            } else {
                collected_out.push(c.to_owned());
            }
        } else {
            let mut words = c.split_whitespace();
            match words.next() {
                Some("@Name:") => {
                    let rest: Vec<_> = words.collect();
                    meta.name = rest.join(" ");
                }
                Some("@Tags:") => {
                    for w in words {
                        meta.tags.insert(w.to_owned());
                    }
                }
                Some("@ExitCode:") => {
                    if let Some(code) = words.next() {
                        let ec:Option<i32> = code.parse().ok();
                        meta.expected_exit_code = ec;
                    }
                }
                Some("@Output:") | Some("@ErrorOut:") => {
                    in_output = true;
                }
                _ => {}
            }
        }
    }
    meta
}

fn update_test(
    test: &PathBuf,
    test_code: &str,
    output: &str,
    error_out: &str,
    status: Option<i32>,
) -> io::Result<()> {
    let mut new_test_code: Vec<String> = Vec::new();
    let mut in_output = false;
    for l in test_code.lines() {
        let word = l.split_whitespace().nth(1);
        match word {
            Some("@Output:") | Some("@ErrorOut:") => {
                in_output = true;
            }
            Some("@EndOutput") | Some("@EndErrorOut") => {
                in_output = false;
            }
            Some("@ExitCode:") => {}
            _ => {
                if !in_output {
                    new_test_code.push(l.to_owned());
                }
            }
        }
    }
    if !output.is_empty() {
        new_test_code.push("// @Output:".to_owned());
        for o in output.lines() {
            new_test_code.push("// ".to_owned() + o);
        }
        new_test_code.push("// @EndOutput".to_owned());
    }
    if !error_out.is_empty() {
        new_test_code.push("// @ErrorOut:".to_owned());
        for o in error_out.lines() {
            new_test_code.push("// ".to_owned() + o);
        }
        new_test_code.push("// @EndErrorOut".to_owned());
    }
    if let Some(code) = status {
        new_test_code.push(format!("// @ExitCode: {}", code));
    }
    let mut file = File::create(test)?;
    let code = new_test_code.join("\n");
    file.write_all(code.as_bytes())
}
