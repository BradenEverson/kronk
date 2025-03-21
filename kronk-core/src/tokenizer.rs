//! Core tokenizer

use std::{error::Error, fmt::Display};

/// A data type that can be tokenized
pub trait Tokenizable {
    /// Tokenize to a token stream
    fn tokenize(&self) -> Result<Vec<Token<'_>>, TokenError>;
}

/// A token in the parsing process
pub enum Token<'a> {
    /// An identifier
    Identifier(&'a str),
    /// A numeric literal
    Literal(f64),
    /// A keyword
    Keyword(Keyword),
    /// {
    OpenBrace,
    /// }
    CloseBrace,
}

/// All reserved keywords
pub enum Keyword {
    /// Variable declaration
    Var,
    /// For loop instantiation
    For,
    /// If statement
    If,
}

#[derive(Debug)]
pub struct TokenError(char);

impl Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token `{}` is invalid :(", self.0)
    }
}

impl Error for TokenError {}

impl<STR> Tokenizable for STR
where
    STR: AsRef<str>,
{
    fn tokenize(&self) -> Result<Vec<Token<'_>>, TokenError> {
        let mut peek = self.as_ref().chars().peekable().enumerate();
        let mut tokens = vec![];

        while let Some((idx, tok)) = peek.next() {
            let next = match tok {
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                bad => return Err(TokenError(bad)),
            };

            tokens.push(next);
        }

        Ok(tokens)
    }
}
