//! Core tokenizer

use std::{error::Error, fmt::Display};

/// A datatype that can be tokenized
pub trait Tokenizable {
    /// Tokenize to a token stream
    fn tokenize(&self) -> Result<Vec<Token<'_>>, TokenError>;
}

/// A token in the parsing process
#[derive(Debug, PartialEq, Clone, Copy)]
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
    /// Semicolon
    Semicolon,

    /// +
    Plus,
    /// -
    Minus,
    /// *
    Mul,
    /// /
    Div,

    /// =
    Equals,

    /// End of file
    EOF,
}

/// All reserved keywords
#[derive(Debug, PartialEq, Clone, Copy)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenError(char, usize);

impl Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token: `{}` at position {} is invalid :(",
            self.0, self.1
        )
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

                '=' => Token::Equals,

                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Mul,
                '/' => Token::Div,

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

                bad => return Err(TokenError(bad, idx)),
            };

            tokens.push(next);
        }

        tokens.push(Token::EOF);

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{Keyword, Token, TokenError};

    use super::Tokenizable;

    #[test]
    fn operators_parsing() {
        let tokens = "var pi = 3.14".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("pi"),
                Token::Equals,
                Token::Literal(3.14),
                Token::EOF
            ]
        )
    }

    #[test]
    fn invalid_characters() {
        let tokens = "foo bar baz ?".tokenize();

        assert_eq!(tokens, Err(TokenError('?', 12)))
    }
}
