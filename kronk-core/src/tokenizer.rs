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
    /// For loop
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
pub struct TokenError {
    /// The invalid token
    token: char,
    /// The line that is invalid
    line: usize,
    /// The column in that line that's invalid
    col: usize,
}

impl TokenError {
    /// Creates a new token error with context
    pub fn new(token: char, line: usize, col: usize) -> Self {
        Self { token, line, col }
    }
}

impl Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Invalid token {} at line {}, col {}",
            self.token, self.line, self.col
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
        let mut line = 1;
        let mut col = 0;

        while let Some((idx, tok)) = peek.next() {
            col += 1;
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

                nl if nl == '\n' => {
                    col = 0;
                    line += 1;
                    continue;
                }

                ws if ws.is_whitespace() => continue,

                num if num.is_numeric() => {
                    let mut curr = String::new();
                    curr.push(num);

                    let mut dot = false;
                    while let Some((_, next)) = peek.peek() {
                        if next.is_numeric() {
                            col += 1;
                            curr.push(peek.next().unwrap().1);
                        } else if *next == '.' && !dot {
                            col += 1;
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
                        if !(next.is_alphabetic() || *next == '_') {
                            break;
                        }

                        end = *idx2;
                        col += 1;
                        peek.next();
                    }

                    let word = &self.as_ref()[idx..=end];
                    if let Ok(keyword) = Keyword::try_from(word) {
                        Token::Keyword(keyword)
                    } else {
                        Token::Identifier(word)
                    }
                }

                bad => return Err(TokenError::new(bad, line, col)),
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

        assert_eq!(tokens, Err(TokenError::new('?', 1, 13)))
    }

    #[test]
    fn parentheses_and_braces() {
        let tokens = "(x + y) { z; }".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Plus,
                Token::Identifier("y"),
                Token::CloseParen,
                Token::OpenBrace,
                Token::Identifier("z"),
                Token::Semicolon,
                Token::CloseBrace,
                Token::EOF
            ]
        );
    }

    #[test]
    fn invalid_tokens() {
        let tokens = "x = @".tokenize();
        assert_eq!(tokens, Err(TokenError::new('@', 1, 5)));

        let tokens = "x = #y".tokenize();
        assert_eq!(tokens, Err(TokenError::new('#', 1, 5)));

        let tokens = "x = y!".tokenize();
        assert_eq!(tokens, Err(TokenError::new('!', 1, 6)));
    }

    #[test]
    fn empty_input() {
        let tokens = "".tokenize().expect("Tokenize");
        assert_eq!(tokens, [Token::EOF]);
    }

    #[test]
    fn keywords_as_identifiers() {
        let tokens = "var varx = forx".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("varx"),
                Token::Equals,
                Token::Identifier("forx"),
                Token::EOF
            ]
        );
    }

    #[test]
    fn long_identifiers() {
        let tokens = "very_long_identifier_name = 123"
            .tokenize()
            .expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Identifier("very_long_identifier_name"),
                Token::Equals,
                Token::Literal(123.0),
                Token::EOF
            ]
        );
    }

    #[test]
    fn whitespace_handling() {
        let tokens = "  var   x  =  123  ".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("x"),
                Token::Equals,
                Token::Literal(123.0),
                Token::EOF
            ]
        );
    }

    #[test]
    fn numeric_literals() {
        let tokens = "123 45.67 0.123 123.".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Literal(123.0),
                Token::Literal(45.67),
                Token::Literal(0.123),
                Token::Literal(123.0),
                Token::EOF
            ]
        );

        let tokens = "123.45.67".tokenize();
        assert!(tokens.is_err());
    }

    #[test]
    fn identifiers_and_keywords() {
        let tokens = "var x = if y".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("x"),
                Token::Equals,
                Token::Keyword(Keyword::If),
                Token::Identifier("y"),
                Token::EOF
            ]
        );

        let tokens = "for var ifelse".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::For),
                Token::Keyword(Keyword::Var),
                Token::Identifier("ifelse"),
                Token::EOF
            ]
        );
    }

    #[test]
    fn arithmetic_expressions() {
        let tokens = "x = (a + b) * c - d / e".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Identifier("x"),
                Token::Equals,
                Token::OpenParen,
                Token::Identifier("a"),
                Token::Plus,
                Token::Identifier("b"),
                Token::CloseParen,
                Token::Mul,
                Token::Identifier("c"),
                Token::Minus,
                Token::Identifier("d"),
                Token::Div,
                Token::Identifier("e"),
                Token::EOF
            ]
        );
    }

    #[test]
    fn multiple_lines_error() {
        let input = r#"var x = 1;
var y = 2;
x = x + $;
    "#;
        let tokens = input.tokenize();
        assert_eq!(tokens, Err(TokenError::new('$', 3, 9)));
    }

    #[test]
    fn multiple_lines() {
        let input = r#"
        var x = 1;
        var y = 2;
        x = x + y;
    "#;
        let tokens = input.tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("x"),
                Token::Equals,
                Token::Literal(1.0),
                Token::Semicolon,
                Token::Keyword(Keyword::Var),
                Token::Identifier("y"),
                Token::Equals,
                Token::Literal(2.0),
                Token::Semicolon,
                Token::Identifier("x"),
                Token::Equals,
                Token::Identifier("x"),
                Token::Plus,
                Token::Identifier("y"),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }
}
