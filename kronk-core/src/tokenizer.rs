//! Core tokenizer

use std::{error::Error, fmt::Display};

/// A datatype that can be tokenized
pub trait Tokenizable {
    /// Tokenize to a token stream
    fn tokenize(&self) -> Result<Vec<Token<'_>>, TokenError>;
}

/// A token tag with additional context like location in the stream for better error logging
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token<'a> {
    /// Token's line
    pub line: usize,
    /// Token's column
    pub col: usize,
    /// Token's length
    pub len: usize,
    /// Token's tag
    pub tag: TokenTag<'a>,
}

/// A token in the parsing process
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenTag<'a> {
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
    /// ++
    PlusPlus,
    /// +=
    PlusEq,
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

impl Display for TokenTag<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::PlusPlus => write!(f, "++"),
            Self::PlusEq => write!(f, "+="),
            Self::Dot => write!(f, "."),
            Self::Semicolon => write!(f, ";"),
            Self::Number(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Identifier(s) => write!(f, "{s}"),
            Self::Keyword(k) => write!(f, "{k}"),
            Self::OpenBrace => write!(f, "{{"),
            Self::CloseBrace => write!(f, "}}"),

            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, ")"),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Slash => write!(f, "/"),
            Self::Star => write!(f, "*"),

            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),

            Self::Bang => write!(f, "!"),
            Self::BangEqual => write!(f, "!="),
            Self::Equal => write!(f, "="),
            Self::EqualEqual => write!(f, "=="),

            Self::Comma => write!(f, ","),
            Self::EOF => write!(f, " "),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::And => write!(f, "and"),
            Self::Class => write!(f, "class"),
            Self::Else => write!(f, "else"),
            Self::False => write!(f, "false"),
            Self::Fun => write!(f, "fun"),
            Self::For => write!(f, "for"),
            Self::If => write!(f, "if"),
            Self::Nil => write!(f, "nil"),
            Self::Or => write!(f, "or"),
            Self::Print => write!(f, "print"),
            Self::Roar => write!(f, "roar"),
            Self::Return => write!(f, "return"),
            Self::Super => write!(f, "super"),
            Self::This => write!(f, "this"),
            Self::True => write!(f, "true"),
            Self::Var => write!(f, "var"),
            Self::While => write!(f, "while"),
        }
    }
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
    /// Roar
    Roar,
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
            "roar" => Ok(Self::Roar),
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
    pub token: char,
    /// The line that is invalid
    pub line: usize,
    /// The column in that line that's invalid
    pub col: usize,
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
            let mut len = 1;
            col += 1;
            let next_tag = match tok {
                '{' => TokenTag::OpenBrace,
                '}' => TokenTag::CloseBrace,

                '(' => TokenTag::OpenParen,
                ')' => TokenTag::CloseParen,

                ';' => TokenTag::Semicolon,
                '.' => TokenTag::Dot,

                '=' => match peek.peek() {
                    Some((_, '=')) => {
                        peek.next();
                        col += 1;
                        len += 1;

                        TokenTag::EqualEqual
                    }
                    _ => TokenTag::Equal,
                },

                '!' => match peek.peek() {
                    Some((_, '=')) => {
                        peek.next();
                        col += 1;
                        len += 1;

                        TokenTag::BangEqual
                    }
                    _ => TokenTag::Bang,
                },

                '<' => match peek.peek() {
                    Some((_, '=')) => {
                        peek.next();
                        col += 1;
                        len += 1;

                        TokenTag::LessEqual
                    }
                    _ => TokenTag::Less,
                },

                '>' => match peek.peek() {
                    Some((_, '=')) => {
                        peek.next();
                        col += 1;
                        len += 1;

                        TokenTag::GreaterEqual
                    }
                    _ => TokenTag::Greater,
                },

                '+' => match peek.peek() {
                    Some((_, '+')) => {
                        peek.next();
                        TokenTag::PlusPlus
                    }
                    Some((_, '=')) => {
                        peek.next();
                        TokenTag::PlusEq
                    }
                    _ => TokenTag::Plus,
                },
                '-' => TokenTag::Minus,
                '*' => TokenTag::Star,
                '/' => match peek.peek() {
                    Some((_, '/')) => {
                        for (_, ch) in peek.by_ref() {
                            if ch == '\n' {
                                break;
                            }
                        }
                        continue;
                    }
                    Some((_, '*')) => {
                        peek.next();
                        while let Some((_, ch)) = peek.next() {
                            if ch == '*' {
                                if let Some((_, '/')) = peek.peek() {
                                    peek.next();
                                    break;
                                }
                            }
                        }

                        continue;
                    }
                    _ => TokenTag::Slash,
                },

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
                            len += 1;
                            curr.push(peek.next().unwrap().1);
                        } else if *next == '.' && !dot {
                            col += 1;
                            len += 1;
                            curr.push(peek.next().unwrap().1);
                            dot = true;
                        } else {
                            break;
                        }
                    }

                    // Unwrap safety, as we build the number we are ensuring that only numeric
                    // characters are added to it, this cannot fail
                    TokenTag::Number(curr.parse().unwrap())
                }

                '"' => {
                    let mut idx2 = idx;
                    let mut ended = false;

                    for (_, c) in peek.by_ref() {
                        if c != '"' {
                            idx2 += 1;
                            col += 1;
                            len += 1;
                        } else {
                            ended = true;
                            break;
                        }
                    }

                    if ended {
                        TokenTag::String(&self.as_ref()[idx + 1..=idx2])
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
                        len += 1;
                        peek.next();
                    }

                    let word = &self.as_ref()[idx..=end];
                    if let Ok(keyword) = Keyword::try_from(word) {
                        TokenTag::Keyword(keyword)
                    } else {
                        TokenTag::Identifier(word)
                    }
                }

                bad => return Err(TokenError::new(bad, line, col)),
            };

            let next = Token {
                line,
                col,
                len,
                tag: next_tag,
            };
            tokens.push(next);
        }

        tokens.push(Token {
            line,
            col,
            len: 0,
            tag: TokenTag::EOF,
        });

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{Keyword, Token, TokenError};

    use super::{TokenTag, Tokenizable};

    fn tags<'a>(toks: Vec<Token<'a>>) -> Vec<TokenTag<'a>> {
        toks.iter().map(|t| t.tag).collect()
    }

    #[test]
    fn operators_parsing() {
        let tokens = "var pi = 3.14".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("pi"),
                TokenTag::Equal,
                TokenTag::Number(3.14),
                TokenTag::EOF
            ]
        )
    }

    #[test]
    fn plus_eq_operator() {
        let tokens = "i+=".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [TokenTag::Identifier("i"), TokenTag::PlusEq, TokenTag::EOF]
        )
    }

    #[test]
    fn inc_operator() {
        let tokens = "i++".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [TokenTag::Identifier("i"), TokenTag::PlusPlus, TokenTag::EOF]
        )
    }

    #[test]
    fn invalid_characters() {
        let tokens = "foo bar baz ?".tokenize();

        assert_eq!(tokens, Err(TokenError::new('?', 1, 13)))
    }

    #[test]
    fn parentheses_and_braces() {
        let tokens = "(x + y) { z; }".tokenize().expect("TokenizeTag");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::OpenParen,
                TokenTag::Identifier("x"),
                TokenTag::Plus,
                TokenTag::Identifier("y"),
                TokenTag::CloseParen,
                TokenTag::OpenBrace,
                TokenTag::Identifier("z"),
                TokenTag::Semicolon,
                TokenTag::CloseBrace,
                TokenTag::EOF
            ]
        );
    }

    #[test]
    fn multi_line_comment() {
        let tokens = r#"
    /*
     This is a multi line comment
     * all of this should be ignored *
     */
    x = 10
            "#
        .tokenize()
        .expect("Tokenize");

        let expected = [
            TokenTag::Identifier("x"),
            TokenTag::Equal,
            TokenTag::Number(10.0),
            TokenTag::EOF,
        ];

        assert_eq!(tags(tokens), expected)
    }

    #[test]
    fn single_line_comment() {
        let tokens = r#"
// This is a comment
    x = 10
            "#
        .tokenize()
        .expect("Tokenize");

        let expected = [
            TokenTag::Identifier("x"),
            TokenTag::Equal,
            TokenTag::Number(10.0),
            TokenTag::EOF,
        ];

        assert_eq!(tags(tokens), expected)
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
        assert_eq!(tags(tokens), [TokenTag::EOF]);
    }

    #[test]
    fn keywords_as_identifiers() {
        let tokens = "var varx = forx".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("varx"),
                TokenTag::Equal,
                TokenTag::Identifier("forx"),
                TokenTag::EOF
            ]
        );
    }

    #[test]
    fn long_identifiers() {
        let tokens = "very_long_identifier_name = 123"
            .tokenize()
            .expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Identifier("very_long_identifier_name"),
                TokenTag::Equal,
                TokenTag::Number(123.0),
                TokenTag::EOF
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
        let tokens = r#""hello!""#.tokenize().expect("TokenizeTag");
        assert_eq!(tags(tokens), [TokenTag::String("hello!"), TokenTag::EOF])
    }

    #[test]
    fn whitespace_handling() {
        let tokens = "  var   x  =  123  ".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Number(123.0),
                TokenTag::EOF
            ]
        );
    }

    #[test]
    fn numeric_literals() {
        let tokens = "123 45.67 0.123 123.".tokenize().expect("TokenizeTag");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Number(123.0),
                TokenTag::Number(45.67),
                TokenTag::Number(0.123),
                TokenTag::Number(123.0),
                TokenTag::EOF
            ]
        );

        let tokens = "123.45.67".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Number(123.45),
                TokenTag::Dot,
                TokenTag::Number(67.0),
                TokenTag::EOF
            ]
        );
    }

    #[test]
    fn identifiers_and_keywords() {
        let tokens = "var x = if y".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Keyword(Keyword::If),
                TokenTag::Identifier("y"),
                TokenTag::EOF
            ]
        );

        let tokens = "for var ifelse".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::For),
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("ifelse"),
                TokenTag::EOF
            ]
        );
    }

    #[test]
    fn arithmetic_expressions() {
        let tokens = "x = (a + b) * c - d / e".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::OpenParen,
                TokenTag::Identifier("a"),
                TokenTag::Plus,
                TokenTag::Identifier("b"),
                TokenTag::CloseParen,
                TokenTag::Star,
                TokenTag::Identifier("c"),
                TokenTag::Minus,
                TokenTag::Identifier("d"),
                TokenTag::Slash,
                TokenTag::Identifier("e"),
                TokenTag::EOF
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
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Number(1.0),
                TokenTag::Semicolon,
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("y"),
                TokenTag::Equal,
                TokenTag::Number(2.0),
                TokenTag::Semicolon,
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Identifier("x"),
                TokenTag::Plus,
                TokenTag::Identifier("y"),
                TokenTag::Semicolon,
                TokenTag::EOF
            ]
        );
    }

    #[test]
    fn edge_cases_for_identifiers() {
        let tokens = "_underscore".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [TokenTag::Identifier("_underscore"), TokenTag::EOF]
        );

        let tokens = "var1".tokenize().expect("Tokenize");
        assert_eq!(tags(tokens), [TokenTag::Identifier("var1"), TokenTag::EOF]);

        let tokens = "var_1".tokenize().expect("Tokenize");
        assert_eq!(tags(tokens), [TokenTag::Identifier("var_1"), TokenTag::EOF]);
    }

    #[test]
    fn edge_cases_for_numbers() {
        let tokens = "0123".tokenize().expect("Tokenize");
        assert_eq!(tags(tokens), [TokenTag::Number(123.0), TokenTag::EOF]);

        let tokens = "123.".tokenize().expect("Tokenize");
        assert_eq!(tags(tokens), [TokenTag::Number(123.0), TokenTag::EOF]);

        let tokens = "123..456".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Number(123.0),
                TokenTag::Dot,
                TokenTag::Number(456.0),
                TokenTag::EOF
            ]
        );
    }

    #[test]
    fn edge_cases_for_operators() {
        let tokens = "x = y + -z".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Identifier("y"),
                TokenTag::Plus,
                TokenTag::Minus,
                TokenTag::Identifier("z"),
                TokenTag::EOF
            ]
        );

        let tokens = "x = y * / z".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Identifier("y"),
                TokenTag::Star,
                TokenTag::Slash,
                TokenTag::Identifier("z"),
                TokenTag::EOF
            ]
        );

        let tokens = "x = y == z".tokenize().expect("TokenizeTag");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Identifier("y"),
                TokenTag::EqualEqual,
                TokenTag::Identifier("z"),
                TokenTag::EOF
            ]
        );
    }

    #[test]
    fn edge_cases_for_whitespace() {
        let tokens = "   var   x   =   123   ".tokenize().expect("TokenizeTag");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Number(123.0),
                TokenTag::EOF
            ]
        );

        let tokens = "\tvar\tx\t=\t123\t".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Number(123.0),
                TokenTag::EOF
            ]
        );

        let tokens = "\nvar\nx\n=\n123\n".tokenize().expect("Tokenize");
        assert_eq!(
            tags(tokens),
            [
                TokenTag::Keyword(Keyword::Var),
                TokenTag::Identifier("x"),
                TokenTag::Equal,
                TokenTag::Number(123.0),
                TokenTag::EOF
            ]
        );
    }
}
