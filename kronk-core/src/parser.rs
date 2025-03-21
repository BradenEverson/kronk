//! Parser definition

use std::{error::Error, fmt::Display};

use crate::tokenizer::Token;

/// A parser holding context
pub struct Parser<'a> {
    /// The token stream being parsed
    tokens: &'a [Token<'a>],
    /// Current index into the token stream
    idx: usize,
}

/// An error that occurs whilst parsing
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ParseError;

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error parsing :(")
    }
}

impl Error for ParseError {}

impl<'a> Parser<'a> {
    /// Constructs a new parser by wrapping over some tokens
    pub fn with_tokens(tokens: &'a [Token<'a>]) -> Self {
        Self { tokens, idx: 0 }
    }

    /// Peeks at the current token, does not advance the index
    #[inline]
    fn peek(&self) -> Token<'a> {
        self.tokens[self.idx]
    }

    /// Consumes the current token assuming it's the provided Token, failing if not
    fn consume(&mut self, token: &Token<'_>) -> Result<(), ParseError> {
        if &self.peek() == token {
            self.idx += 1;
            Ok(())
        } else {
            Err(ParseError)
        }
    }

    /// Checks if stream is finished
    fn at_end(&self) -> bool {
        self.peek() == Token::EOF
    }

    /// Advances forward and returns the token we hop after
    fn advance(&mut self) -> Token<'a> {
        if !self.at_end() {
            self.idx += 1
        }

        self.tokens[self.idx - 1]
    }

    /// Parses the current token stream
    pub fn parse(&mut self) -> Result<Expr<'a>, ParseError> {
        let expr = self.expression()?;
        self.consume(&Token::EOF)?;
        Ok(expr)
    }

    /// -> equality ;
    fn expression(&mut self) -> Result<Expr<'a>, ParseError> {
        self.equality()
    }

    /// -> comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.comparison()?;

        while matches!(self.peek(), Token::BangEqual | Token::EqualEqual) {
            let op = match self.advance() {
                Token::BangEqual => BinaryOperator::Neq,
                Token::EqualEqual => BinaryOperator::Eq,
                _ => unreachable!(),
            };

            let right = self.comparison()?;

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    /// -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.term()?;

        while matches!(
            self.peek(),
            Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual
        ) {
            let op = match self.advance() {
                Token::GreaterEqual => BinaryOperator::Gte,
                Token::Greater => BinaryOperator::Gt,
                Token::LessEqual => BinaryOperator::Lte,
                Token::Less => BinaryOperator::Lt,
                _ => unreachable!(),
            };

            let right = self.term()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    /// -> factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.factor()?;

        while matches!(self.peek(), Token::Plus | Token::Minus) {
            let op = match self.advance() {
                Token::Minus => BinaryOperator::Sub,
                Token::Plus => BinaryOperator::Add,
                _ => unreachable!(),
            };

            let right = self.factor()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    /// ->  unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.unary()?;

        while matches!(self.peek(), Token::Slash | Token::Star) {
            let op = match self.advance() {
                Token::Slash => BinaryOperator::Div,
                Token::Star => BinaryOperator::Mul,
                _ => unreachable!(),
            };

            let right = self.unary()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    /// -> ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<Expr<'a>, ParseError> {
        if matches!(self.peek(), Token::Bang | Token::Minus) {
            let op = match self.advance() {
                Token::Bang => UnaryOperator::Not,
                Token::Minus => UnaryOperator::Neg,
                _ => unreachable!(),
            };

            let unary = self.unary()?;

            Ok(Expr::Unary {
                op,
                node: Box::new(unary),
            })
        } else {
            self.primary()
        }
    }

    /// NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr<'a>, ParseError> {
        match self.advance() {
            Token::Number(n) => Ok(Expr::Literal(Literal::Number(n))),
            Token::String("true") => Ok(Expr::Literal(Literal::True)),
            Token::String("false") => Ok(Expr::Literal(Literal::True)),
            Token::String("nil") => Ok(Expr::Literal(Literal::Nil)),
            Token::String(s) => Ok(Expr::Literal(Literal::String(s))),
            Token::OpenParen => {
                let expr = self.expression()?;
                self.consume(&Token::CloseParen)?;

                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => Err(ParseError),
        }
    }
}

/// An expression node in the AST
#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    /// A literal
    Literal(Literal<'a>),
    /// Unary operation
    Unary {
        /// Operator
        op: UnaryOperator,
        /// Expression Node
        node: Box<Expr<'a>>,
    },
    /// Binary operation
    Binary {
        /// Operator
        op: BinaryOperator,
        /// Left expression node
        left: Box<Expr<'a>>,
        /// Right expression node
        right: Box<Expr<'a>>,
    },
    /// A grouping ()
    Grouping(Box<Expr<'a>>),
}

/// A literal value
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Literal<'a> {
    /// A number
    Number(f64),
    /// A string
    String(&'a str),
    /// Boolean true
    True,
    /// Boolean false
    False,
    /// NULL
    Nil,
}

/// An unary operator
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    /// Negation
    Neg,
    /// Not
    Not,
}

/// A binary operator
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    /// Equality
    Eq,
    /// Inequality
    Neq,
    /// Greater than
    Gt,
    /// Greater than or equal
    Gte,
    /// Less than
    Lt,
    /// Less than or equal
    Lte,
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
}
