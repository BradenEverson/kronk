//! Main interpretter REPL runtime for Kronk programs

use std::{env, fs::File, io::Read};

use kronk_core::{eval::Interpretter, parser::Parser, tokenizer::Tokenizable};

fn main() {
    let file: String = env::args().skip(1).collect();
    let mut file = File::open(file).expect("The provided file does not exist");
    let mut buf = String::new();

    let mut interp = Interpretter::default();

    file.read_to_string(&mut buf)
        .expect("Failed to read provided file");

    let tokens = buf
        .tokenize()
        .map_err(|e| panic!("Error Tokenizing: {e}"))
        .unwrap();
    let mut parser = Parser::with_tokens(&tokens);
    let ast = parser
        .parse_many()
        .map_err(|e| panic!("Error Parsing: {e}"))
        .unwrap();

    for ast in ast {
        interp
            .eval(ast)
            .map_err(|e| panic!("Runtime Error: {e}"))
            .unwrap();
    }
}
