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
    Number(f64),
    /// A string literal
    String(&'a str),
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
    /// ;
    Semicolon,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Star,
    ///,
    Comma,
    /// .
    Dot,
    /// /
    Slash,
    /// =
    Equal,
    /// >
    Greater,
    /// >=
    GreaterEqual,
    /// ==
    EqualEqual,
    /// !=
    BangEqual,
    /// !
    Bang,
    /// <
    Less,
    /// <=
    LessEqual,

    /// End of file
    EOF,
}

/// All reserved keywords
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    /// and
    And,
    /// class
    Class,
    /// else
    Else,
    /// false
    False,
    /// fun
    Fun,
    /// for
    For,
    /// if
    If,
    /// nil
    Nil,
    /// or
    Or,
    /// print
    Print,
    /// return
    Return,
    /// super
    Super,
    /// this
    This,
    /// true
    True,
    /// var
    Var,
    /// while
    While,
}

impl TryFrom<&str> for Keyword {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "and" => Ok(Self::And),
            "class" => Ok(Self::Class),
            "else" => Ok(Self::Else),
            "false" => Ok(Self::False),
            "fun" => Ok(Self::Fun),
            "for" => Ok(Self::For),
            "if" => Ok(Self::If),
            "nil" => Ok(Self::Nil),
            "or" => Ok(Self::Or),
            "print" => Ok(Self::Print),
            "return" => Ok(Self::Return),
            "super" => Ok(Self::Super),
            "this" => Ok(Self::This),
            "true" => Ok(Self::True),
            "var" => Ok(Self::Var),
            "while" => Ok(Self::While),
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
                '.' => Token::Dot,

                '=' => match peek.peek() {
                    Some((_, '=')) => {
                        peek.next();
                        col += 1;

                        Token::EqualEqual
                    }
                    _ => Token::Equal,
                },

                '!' => match peek.peek() {
                    Some((_, '=')) => {
                        peek.next();
                        col += 1;

                        Token::BangEqual
                    }
                    _ => Token::Bang,
                },

                '<' => match peek.peek() {
                    Some((_, '=')) => {
                        peek.next();
                        col += 1;

                        Token::LessEqual
                    }
                    _ => Token::Less,
                },

                '>' => match peek.peek() {
                    Some((_, '=')) => {
                        peek.next();
                        col += 1;

                        Token::GreaterEqual
                    }
                    _ => Token::Greater,
                },

                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,

                '\n' => {
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
                    Token::Number(curr.parse().unwrap())
                }

                '"' => {
                    let mut idx2 = idx;
                    let mut ended = false;

                    for (_, c) in peek.by_ref() {
                        if c != '"' {
                            idx2 += 1;
                            col += 1;
                        } else {
                            ended = true;
                            break;
                        }
                    }

                    if ended {
                        Token::String(&self.as_ref()[idx + 1..=idx2])
                    } else {
                        return Err(TokenError::new('"', line, col));
                    }
                }

                ch if ch.is_alphanumeric() || ch == '_' => {
                    let mut end = idx;

                    while let Some((idx2, next)) = peek.peek() {
                        if !(next.is_alphanumeric() || *next == '_') {
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
                Token::Equal,
                Token::Number(3.14),
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
                Token::Equal,
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
                Token::Equal,
                Token::Number(123.0),
                Token::EOF
            ]
        );
    }

    #[test]
    fn bad_string() {
        let tokens = r#""hello!"#.tokenize();
        assert!(tokens.is_err())
    }

    #[test]
    fn string() {
        let tokens = r#""hello!""#.tokenize().expect("Tokenize");
        assert_eq!(tokens, [Token::String("hello!"), Token::EOF])
    }

    #[test]
    fn whitespace_handling() {
        let tokens = "  var   x  =  123  ".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("x"),
                Token::Equal,
                Token::Number(123.0),
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
                Token::Number(123.0),
                Token::Number(45.67),
                Token::Number(0.123),
                Token::Number(123.0),
                Token::EOF
            ]
        );

        let tokens = "123.45.67".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Number(123.45),
                Token::Dot,
                Token::Number(67.0),
                Token::EOF
            ]
        );
    }

    #[test]
    fn identifiers_and_keywords() {
        let tokens = "var x = if y".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("x"),
                Token::Equal,
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
                Token::Equal,
                Token::OpenParen,
                Token::Identifier("a"),
                Token::Plus,
                Token::Identifier("b"),
                Token::CloseParen,
                Token::Star,
                Token::Identifier("c"),
                Token::Minus,
                Token::Identifier("d"),
                Token::Slash,
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
                Token::Equal,
                Token::Number(1.0),
                Token::Semicolon,
                Token::Keyword(Keyword::Var),
                Token::Identifier("y"),
                Token::Equal,
                Token::Number(2.0),
                Token::Semicolon,
                Token::Identifier("x"),
                Token::Equal,
                Token::Identifier("x"),
                Token::Plus,
                Token::Identifier("y"),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    #[test]
    fn edge_cases_for_identifiers() {
        let tokens = "_underscore".tokenize().expect("Tokenize");
        assert_eq!(tokens, [Token::Identifier("_underscore"), Token::EOF]);

        let tokens = "var1".tokenize().expect("Tokenize");
        assert_eq!(tokens, [Token::Identifier("var1"), Token::EOF]);

        let tokens = "var_1".tokenize().expect("Tokenize");
        assert_eq!(tokens, [Token::Identifier("var_1"), Token::EOF]);
    }

    #[test]
    fn edge_cases_for_numbers() {
        let tokens = "0123".tokenize().expect("Tokenize");
        assert_eq!(tokens, [Token::Number(123.0), Token::EOF]);

        let tokens = "123.".tokenize().expect("Tokenize");
        assert_eq!(tokens, [Token::Number(123.0), Token::EOF]);

        let tokens = "123..456".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Number(123.0),
                Token::Dot,
                Token::Number(456.0),
                Token::EOF
            ]
        );
    }

    #[test]
    fn edge_cases_for_operators() {
        let tokens = "x = y + -z".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Identifier("x"),
                Token::Equal,
                Token::Identifier("y"),
                Token::Plus,
                Token::Minus,
                Token::Identifier("z"),
                Token::EOF
            ]
        );

        let tokens = "x = y * / z".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Identifier("x"),
                Token::Equal,
                Token::Identifier("y"),
                Token::Star,
                Token::Slash,
                Token::Identifier("z"),
                Token::EOF
            ]
        );

        let tokens = "x = y == z".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Identifier("x"),
                Token::Equal,
                Token::Identifier("y"),
                Token::EqualEqual,
                Token::Identifier("z"),
                Token::EOF
            ]
        );
    }

    #[test]
    fn edge_cases_for_whitespace() {
        let tokens = "   var   x   =   123   ".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("x"),
                Token::Equal,
                Token::Number(123.0),
                Token::EOF
            ]
        );

        let tokens = "\tvar\tx\t=\t123\t".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("x"),
                Token::Equal,
                Token::Number(123.0),
                Token::EOF
            ]
        );

        let tokens = "\nvar\nx\n=\n123\n".tokenize().expect("Tokenize");
        assert_eq!(
            tokens,
            [
                Token::Keyword(Keyword::Var),
                Token::Identifier("x"),
                Token::Equal,
                Token::Number(123.0),
                Token::EOF
            ]
        );
    }
}
