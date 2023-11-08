use std::env;
use std::fmt::{Display, Formatter, Result};
use std::fs;
use Solution::*;

pub mod helpers;

pub const ANSI_ITALIC: &str = "\x1b[3m";
pub const ANSI_BOLD: &str = "\x1b[1m";
pub const ANSI_RESET: &str = "\x1b[0m";

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum Solution {
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),
    Str(String),
    Empty,
}

impl Display for Solution {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            I16(x) => x.fmt(f),
            I32(x) => x.fmt(f),
            I64(x) => x.fmt(f),
            I128(x) => x.fmt(f),
            U16(x) => x.fmt(f),
            U32(x) => x.fmt(f),
            U64(x) => x.fmt(f),
            U128(x) => x.fmt(f),
            USize(x) => x.fmt(f),
            Str(x) => x.fmt(f),
            Empty => write!(f, "Not implemented"),
        }
    }
}

#[macro_export]
macro_rules! solve {
    ($part:expr, $solver:ident, $input:expr) => {{
        use advent_of_code::{ANSI_BOLD, ANSI_ITALIC, ANSI_RESET};
        use std::fmt::Display;
        use std::time::Instant;

        fn print_result(func: impl FnOnce(&str) -> Solution, input: &str) {
            let timer = Instant::now();
            let result = func(input);
            let elapsed = timer.elapsed();
            println!(
                        "{} {}(elapsed: {:.2?}){}",
                        result, ANSI_ITALIC, elapsed, ANSI_RESET
                    );
        }

        println!("🎄 {}Part {}{} 🎄", ANSI_BOLD, $part, ANSI_RESET);
        print_result($solver, $input);
    }};
}

pub fn read_file(folder: &str, day: u8) -> String {
    let cwd = env::current_dir().unwrap();

    let filepath = cwd.join("src").join(folder).join(format!("{day:02}.txt"));

    let f = fs::read_to_string(filepath);
    f.expect("could not open input file")
}

fn parse_time(val: &str, postfix: &str) -> f64 {
    val.split(postfix).next().unwrap().parse().unwrap()
}

pub fn parse_exec_time(output: &str) -> f64 {
    output.lines().fold(0_f64, |acc, l| {
        if !l.contains("elapsed:") {
            acc
        } else {
            let timing = l.split("(elapsed: ").last().unwrap();
            // use `contains` istd. of `ends_with`: string may contain ANSI escape sequences.
            // for possible time formats, see: https://github.com/rust-lang/rust/blob/1.64.0/library/core/src/time.rs#L1176-L1200
            if timing.contains("ns)") {
                acc // range below rounding precision.
            } else if timing.contains("µs)") {
                acc + parse_time(timing, "µs") / 1000_f64
            } else if timing.contains("ms)") {
                acc + parse_time(timing, "ms")
            } else if timing.contains("s)") {
                acc + parse_time(timing, "s") * 1000_f64
            } else {
                acc
            }
        }
    })
}

/// copied from: https://github.com/rust-lang/rust/blob/1.64.0/library/std/src/macros.rs#L328-L333
#[cfg(test)]
macro_rules! assert_approx_eq {
    ($a:expr, $b:expr) => {{
        let (a, b) = (&$a, &$b);
        assert!(
            (*a - *b).abs() < 1.0e-6,
            "{} is not approximately equal to {}",
            *a,
            *b
        );
    }};
}

pub mod aoc_cli {
    use std::{
        fmt::Display,
        process::{Command, Output, Stdio},
    };

    pub enum AocCliError {
        CommandNotFound,
        CommandNotCallable,
        BadExitStatus(Output),
        IoError,
    }

    impl Display for AocCliError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                AocCliError::CommandNotFound => write!(f, "aoc-cli is not present in environment."),
                AocCliError::CommandNotCallable => write!(f, "aoc-cli could not be called."),
                AocCliError::BadExitStatus(_) => {
                    write!(f, "aoc-cli exited with a non-zero status.")
                }
                AocCliError::IoError => write!(f, "could not write output files to file system."),
            }
        }
    }

    pub fn check() -> Result<(), AocCliError> {
        Command::new("aoc")
            .arg("-V")
            .output()
            .map_err(|_| AocCliError::CommandNotFound)?;
        Ok(())
    }

    pub fn download(day: u8, year: u16) -> Result<Output, AocCliError> {
        let input_path = get_input_path(day);

        let args = build_args(
            "download",
            &[
                "--overwrite".into(),
                "--input-file".into(),
                input_path.to_string(),
                "-I".to_string(),
            ],
            day,
            year,
        );

        let output = call_aoc_cli(&args)?;

        println!("{:?}", output);

        if output.status.success() {
            println!("---");
            println!("🎄 Successfully wrote input to \"{}\".", &input_path);
            Ok(output)
        } else {
            Err(AocCliError::BadExitStatus(output))
        }
    }

    fn get_input_path(day: u8) -> String {
        let day_padded = format!("{day:02}");
        format!("src/inputs/{day_padded}.txt")
    }

    fn get_puzzle_path(day: u8) -> String {
        let day_padded = format!("{day:02}");
        format!("src/puzzles/{day_padded}.md")
    }

    fn build_args(command: &str, args: &[String], day: u8, year: u16) -> Vec<String> {
        let mut cmd_args = args.to_vec();

        cmd_args.push("--year".into());
        cmd_args.push(year.to_string());
        cmd_args.append(&mut vec!["--day".into(), day.to_string(), command.into()]);

        cmd_args
    }

    fn call_aoc_cli(args: &[String]) -> Result<Output, AocCliError> {
        if cfg!(debug_assertions) {
            println!("Calling >aoc with: {}", args.join(" "));
        }

        Command::new("aoc")
            .args(args)
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .output()
            .map_err(|_| AocCliError::CommandNotCallable)
    }
}