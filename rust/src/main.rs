use advent_of_code::{ANSI_BOLD, ANSI_ITALIC, ANSI_RESET};
use std::process::Command;
use advent_of_code::aoc_cli;
use std::{
    env,
    fs::{File, OpenOptions},
    io::Write,
};

fn main() {
    let mut args = pico_args::Arguments::from_env();
    let command: String = args.free_from_str().unwrap_or("all".to_string());

    match command.as_str() {
        "all" => solve_all(),
        "scaffold" => scaffold(args),
        "download" => download(args),
        _ => {
            eprintln!("Command '{}' unknown.", command);
            panic!()
        }
    }
}

fn solve_all() {
    let total: f64 = (1..=25)
        .map(|day| {
            let day = format!("{day:02}");

            let mut args = vec!["run", "--bin", &day];
            if cfg!(not(debug_assertions)) {
                args.push("--release");
            }

            let cmd = Command::new("cargo").args(&args).output().unwrap();

            println!("----------");
            println!("{ANSI_BOLD}| Day {day} |{ANSI_RESET}");
            println!("----------");

            let output = String::from_utf8(cmd.stdout).unwrap();
            let is_empty = output.is_empty();

            println!(
                "{}",
                if is_empty {
                    "Not solved."
                } else {
                    output.trim()
                }
            );

            if is_empty {
                0_f64
            } else {
                advent_of_code::parse_exec_time(&output)
            }
        })
        .sum();

    println!("{ANSI_BOLD}Total:{ANSI_RESET} {ANSI_ITALIC}{total:.2}ms{ANSI_RESET}");
}

fn download(mut a: pico_args::Arguments) {
    let day: u8 = match a.free_from_str() {
        Ok(day) => day,
        Err(e) => {
            eprintln!("Failed to process arguments: {e}");
            panic!();
        }
    };

    if aoc_cli::check().is_err() {
        eprintln!("command \"aoc\" not found or not callable. Try running \"cargo install aoc-cli --version 0.7.0\" to install it.");
        panic!();
    }

    let year = get_year_env();

    match aoc_cli::download(day, year) {
        Ok(cmd_output) => {
            if !cmd_output.status.success() {
                panic!();
            }
        }
        Err(e) => {
            eprintln!("failed to spawn aoc-cli: {e}");
            panic!();
        }
    }
}

const MODULE_TEMPLATE: &str = r###"use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    Solution::Empty
}

pub fn part_two(input: &str) -> Solution {
    Solution::Empty
}

fn main() {
    let input = &advent_of_code::read_file("inputs", DAY);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", DAY);
        assert_eq!(part_one(&input), Solution::Empty);
        assert_eq!(part_two(&input), Solution::Empty);
    }
}
"###;

fn scaffold(mut a: pico_args::Arguments) {
    let day: u8 = match a.free_from_str() {
        Ok(day) => day,
        Err(_) => {
            eprintln!("Need to specify a day (as integer). example: `cargo scaffold 7`");
            panic!();
        }
    };

    let day_padded = format!("{day:02}");

    let input_path = format!("src/inputs/{day_padded}.txt");
    let example_path = format!("src/examples/{day_padded}.txt");
    let module_path = format!("src/bin/{day_padded}.rs");

    let mut file = match safe_create_file(&module_path) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Failed to create module file: {e}");
            panic!();
        }
    };

    match file.write_all(MODULE_TEMPLATE.replace("DAY", &day.to_string()).as_bytes()) {
        Ok(_) => {
            println!("Created module file \"{}\"", &module_path);
        }
        Err(e) => {
            eprintln!("Failed to write module contents: {e}");
            panic!();
        }
    }

    match create_file(&input_path) {
        Ok(_) => {
            println!("Created empty input file \"{}\"", &input_path);
        }
        Err(e) => {
            eprintln!("Failed to create input file: {e}");
            panic!();
        }
    }

    match create_file(&example_path) {
        Ok(_) => {
            println!("Created empty example file \"{}\"", &example_path);
        }
        Err(e) => {
            eprintln!("Failed to create example file: {e}");
            panic!();
        }
    }

    println!("---");
    println!(
        "🎄 Type `cargo solve {}` to run your solution.",
        &day_padded
    );
}

fn get_year_env() -> u16 {
    match env::var("AOC_YEAR") {
        Ok(y) => y.parse::<u16>().unwrap(),
        Err(_) => {
            eprintln!("Please set the 'AOC_YEAR' environment variable inside the cargo config");
            panic!()
        }
    }
}

fn safe_create_file(path: &str) -> Result<File, std::io::Error> {
    OpenOptions::new().write(true).create_new(true).open(path)
}

fn create_file(path: &str) -> Result<File, std::io::Error> {
    OpenOptions::new().write(true).create(true).open(path)
}
