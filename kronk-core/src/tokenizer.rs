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
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// SemiColon
    Semicolon,
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

impl TryFrom<&str> for Keyword {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "var" => Ok(Self::Var),
            "for" => Ok(Self::For),
            "if" => Ok(Self::If),
            _ => Err(()),
        }
    }
}

/// An error during tokenization
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
        let mut peek = self.as_ref().chars().enumerate().peekable();
        let mut tokens = vec![];

        while let Some((idx, tok)) = peek.next() {
            let next = match tok {
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,

                '(' => Token::OpenParen,
                ')' => Token::CloseParen,

                ';' => Token::Semicolon,

                ws if ws.is_whitespace() => continue,

                num if num.is_numeric() => {
                    let mut curr = String::new();
                    curr.push(num);

                    let mut dot = false;
                    while let Some((_, next)) = peek.peek() {
                        if next.is_numeric() {
                            curr.push(peek.next().unwrap().1);
                        } else if *next == '.' && !dot {
                            curr.push(peek.next().unwrap().1);
                            dot = true;
                        } else {
                            break;
                        }
                    }

                    // Unwrap safety, as we build the number we are ensuring that only numeric
                    // characters are added to it, this cannot fail
                    Token::Literal(curr.parse().unwrap())
                }

                ch if ch.is_alphanumeric() => {
                    let mut end = idx;

                    while let Some((idx2, next)) = peek.peek() {
                        if !next.is_alphabetic() {
                            break;
                        }

                        end = *idx2;
                        peek.next();
                    }

                    let word = &self.as_ref()[idx..=end];
                    if let Ok(keyword) = Keyword::try_from(word) {
                        Token::Keyword(keyword)
                    } else {
                        Token::Identifier(word)
                    }
                }

                bad => return Err(TokenError(bad)),
            };

            tokens.push(next);
        }

        Ok(tokens)
    }
}
