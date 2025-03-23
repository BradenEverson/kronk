//! Main interpretter REPL runtime for Kronk programs

use std::{env, fs::File, io::Read, process};

use kronk_core::{eval::Interpretter, parser::Parser, tokenizer::Tokenizable};

/// The current version
const VERSION: &str = "0.1.0";

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_help();
        process::exit(0);
    }

    match args[1].as_str() {
        "--help" | "-h" => print_help(),
        "--version" | "-v" => print_version(),
        file_path => run_file(file_path),
    }
}

/// Prints some useful help info
fn print_help() {
    println!("Kronk Interpreter {}", VERSION);
    println!("Usage: kronk [OPTIONS] [FILE]");
    println!();
    println!("Options:");
    println!("  -h, --help       Print this help message");
    println!("  -v, --version    Print the version information");
    println!();
    println!("Provide a file to interpret Kronk code.");
}

/// Prints the version
fn print_version() {
    println!("Kronk Interpreter {}", VERSION);
}

/// Runs a file for KRONK interpretation
fn run_file(file_path: &str) {
    let mut file = match File::open(file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            process::exit(1);
        }
    };

    let mut buf = String::new();
    if let Err(err) = file.read_to_string(&mut buf) {
        eprintln!("Error reading file: {}", err);
        process::exit(1);
    }

    let tokens = match buf.tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("Error tokenizing: {}", err);
            process::exit(1);
        }
    };

    let mut parser = Parser::with_tokens(&tokens);
    let ast = match parser.parse_many() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Error parsing: {}", err);
            process::exit(1);
        }
    };

    let mut interp = Interpretter::default();
    for node in ast {
        if let Err(err) = interp.eval(node) {
            eprintln!("Runtime error: {}", err);
            process::exit(1);
        }
    }
}
