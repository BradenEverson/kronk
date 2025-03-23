//! Parser definition

use std::{error::Error, fmt::Display};

use crate::tokenizer::{Keyword, Token};

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

        if self.idx == 0 {
            self.tokens[0]
        } else {
            self.tokens[self.idx - 1]
        }
    }

    /// Parses a token stream into a sequence of statements
    pub fn parse_many(&mut self) -> Result<Vec<Expr<'a>>, ParseError> {
        let mut expressions = vec![];

        while self.peek() != Token::EOF {
            let expr = self.parse()?;
            self.consume(&Token::Semicolon)?;
            expressions.push(expr);
        }

        Ok(expressions)
    }

    /// Parses the current token stream
    pub fn parse(&mut self) -> Result<Expr<'a>, ParseError> {
        let expr = self.statement()?;
        Ok(expr)
    }

    /// A statement is either `print `expression` | `expression` ;
    fn statement(&mut self) -> Result<Expr<'a>, ParseError> {
        // TODO, this here is where we should be expecting and
        // consuming semicolons
        match self.peek() {
            Token::Keyword(Keyword::Print) => {
                self.advance();
                let next = self.expression()?;

                Ok(Expr::Print(Box::new(next)))
            }
            Token::OpenBrace => self.block(),
            Token::Keyword(Keyword::If) => self.if_statement(),
            Token::Keyword(Keyword::While) => self.while_statement(),
            _ => self.expression(),
        }
    }

    /// A while statement is `while ( `equality` ) `expression``
    fn while_statement(&mut self) -> Result<Expr<'a>, ParseError> {
        self.consume(&Token::Keyword(Keyword::While))?;
        self.consume(&Token::OpenParen)?;

        let condition = Box::new(self.equality()?);

        self.consume(&Token::CloseParen)?;

        let exec = Box::new(self.statement()?);

        Ok(Expr::WhileLoop { condition, exec })
    }

    /// A block is `{ (expression)* }`
    fn block(&mut self) -> Result<Expr<'a>, ParseError> {
        self.consume(&Token::OpenBrace)?;
        let mut block_items = vec![];

        while self.peek() != Token::CloseBrace {
            let expr = self.statement()?;
            self.consume(&Token::Semicolon)?;
            block_items.push(expr);
        }

        self.consume(&Token::CloseBrace)?;

        Ok(Expr::Block(block_items))
    }

    /// An If statement is:
    /// `if ( equality ) { `block` } ( else { `block` })?`
    fn if_statement(&mut self) -> Result<Expr<'a>, ParseError> {
        self.consume(&Token::Keyword(Keyword::If))?;
        self.consume(&Token::OpenParen)?;
        let check = self.equality()?;
        self.consume(&Token::CloseParen)?;
        let block = self.block()?;

        let mut else_branch = None;

        if self.peek() == Token::Keyword(Keyword::Else) {
            self.advance();
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Expr::Conditional {
            condition: Box::new(check),
            true_branch: Box::new(block),
            else_branch,
        })
    }

    /// -> equality  | var ident = equality;
    fn expression(&mut self) -> Result<Expr<'a>, ParseError> {
        if self.peek() == Token::Keyword(Keyword::Var) {
            self.advance();
            if let Token::Identifier(name) = self.advance() {
                self.consume(&Token::Equal)?;
                let val = self.equality()?;
                Ok(Expr::Assignment {
                    name,
                    val: Box::new(val),
                })
            } else {
                Err(ParseError)
            }
        } else {
            self.equality()
        }
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

    /// Variable | NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr<'a>, ParseError> {
        match self.advance() {
            Token::Number(n) => Ok(Expr::Literal(Literal::Number(n))),
            Token::Keyword(Keyword::True) => Ok(Expr::Literal(Literal::True)),
            Token::Keyword(Keyword::False) => Ok(Expr::Literal(Literal::False)),
            Token::Keyword(Keyword::Nil) => Ok(Expr::Literal(Literal::Nil)),
            Token::Identifier(ident) => Ok(Expr::Variable(ident)),
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
    /// A while style loop that will repeat until the inner condition is false
    WhileLoop {
        /// Condition being checked
        condition: Box<Expr<'a>>,
        /// Body being executed until then
        exec: Box<Expr<'a>>,
    },
    /// A conditional executor
    Conditional {
        /// The condition being checked
        condition: Box<Expr<'a>>,
        /// What is executed if the condition is true
        true_branch: Box<Expr<'a>>,
        /// What is optionally executed if else
        else_branch: Option<Box<Expr<'a>>>,
    },
    /// A block to be executed
    Block(Vec<Expr<'a>>),
    /// A literal
    Literal(Literal<'a>),
    /// A variable
    Variable(&'a str),
    /// Unary operation
    Unary {
        /// Operator
        op: UnaryOperator,
        /// Expression Node
        node: Box<Expr<'a>>,
    },
    /// Print the expressions result to stdout
    Print(Box<Expr<'a>>),
    /// Assignment operator
    Assignment {
        /// The variable name
        name: &'a str,
        /// The variable value
        val: Box<Expr<'a>>,
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
#[derive(PartialEq, Debug, Clone)]
pub enum Literal<'a> {
    /// A number
    Number(f64),
    /// A string
    String(&'a str),
    /// Concatenated strings
    Concat(Box<Literal<'a>>, Box<Literal<'a>>),
    /// Boolean true
    True,
    /// Boolean false
    False,
    /// NULL
    Nil,
    /// A void return
    Void,
}

impl Display for Literal<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "{s}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Nil => write!(f, "nil"),
            Self::Concat(a, b) => write!(f, "{a}{b}"),
            Self::Void => write!(f, ""),
        }
    }
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

#[cfg(test)]
mod tests {
    use crate::{
        parser::{BinaryOperator, Expr, Literal, Parser, UnaryOperator},
        tokenizer::Tokenizable,
    };

    #[test]
    fn assignment() {
        let tokens = "var foo = 100".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Assignment {
                name: "foo",
                val: Box::new(Expr::Literal(Literal::Number(100.0)))
            }
        )
    }

    #[test]
    fn basic_ast_generated() {
        let tokens = "100 + 100".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOperator::Add,
                left: Box::new(Expr::Literal(Literal::Number(100.0))),
                right: Box::new(Expr::Literal(Literal::Number(100.0)))
            }
        )
    }

    #[test]
    fn while_loop_structure() {
        let tokens = "while (true) {};".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::WhileLoop {
                condition: Box::new(Expr::Literal(Literal::True)),
                exec: Box::new(Expr::Block(vec![]))
            }
        )
    }

    #[test]
    fn test_literal_number() {
        let tokens = "42".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(ast, Expr::Literal(Literal::Number(42.0)));
    }

    #[test]
    fn test_literal_string() {
        let tokens = "\"hello\"".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(ast, Expr::Literal(Literal::String("hello")));
    }

    #[test]
    fn test_literal_true() {
        let tokens = "true".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(ast, Expr::Literal(Literal::True));
    }

    #[test]
    fn empty_block() {
        let tokens = "{}".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(ast, Expr::Block(vec![]));
    }

    #[test]
    fn if_block_simple() {
        let tokens = r#"
            if (true) {
                print "Hello!";
            };
        "#
        .tokenize()
        .expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Conditional {
                condition: Box::new(Expr::Literal(Literal::True)),
                true_branch: Box::new(Expr::Block(vec![Expr::Print(Box::new(Expr::Literal(
                    Literal::String("Hello!")
                )))])),
                else_branch: None,
            }
        );
    }

    #[test]
    fn block_with_stuff() {
        let tokens = r#"
        {
            var foo = 10;
            print foo;
        }"#
        .tokenize()
        .expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Block(vec![
                Expr::Assignment {
                    name: "foo",
                    val: Box::new(Expr::Literal(Literal::Number(10.0)))
                },
                Expr::Print(Box::new(Expr::Variable("foo")))
            ])
        );
    }

    #[test]
    fn test_literal_false() {
        let tokens = "false".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(ast, Expr::Literal(Literal::False));
    }

    #[test]
    fn test_literal_nil() {
        let tokens = "nil".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(ast, Expr::Literal(Literal::Nil));
    }

    #[test]
    fn test_unary_negation() {
        let tokens = "-42".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Unary {
                op: UnaryOperator::Neg,
                node: Box::new(Expr::Literal(Literal::Number(42.0))),
            }
        );
    }

    #[test]
    fn test_unary_not() {
        let tokens = "!true".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Unary {
                op: UnaryOperator::Not,
                node: Box::new(Expr::Literal(Literal::True)),
            }
        );
    }

    #[test]
    fn test_binary_addition() {
        let tokens = "100 + 200".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOperator::Add,
                left: Box::new(Expr::Literal(Literal::Number(100.0))),
                right: Box::new(Expr::Literal(Literal::Number(200.0))),
            }
        );
    }

    #[test]
    fn test_binary_multiplication() {
        let tokens = "3 * 4".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOperator::Mul,
                left: Box::new(Expr::Literal(Literal::Number(3.0))),
                right: Box::new(Expr::Literal(Literal::Number(4.0))),
            }
        );
    }

    #[test]
    fn test_binary_equality() {
        let tokens = "true == false".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOperator::Eq,
                left: Box::new(Expr::Literal(Literal::True)),
                right: Box::new(Expr::Literal(Literal::False)),
            }
        );
    }

    #[test]
    fn test_binary_inequality() {
        let tokens = "true != false".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOperator::Neq,
                left: Box::new(Expr::Literal(Literal::True)),
                right: Box::new(Expr::Literal(Literal::False)),
            }
        );
    }

    #[test]
    fn test_grouping() {
        let tokens = "(42 + 10) * 2".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOperator::Mul,
                left: Box::new(Expr::Grouping(Box::new(Expr::Binary {
                    op: BinaryOperator::Add,
                    left: Box::new(Expr::Literal(Literal::Number(42.0))),
                    right: Box::new(Expr::Literal(Literal::Number(10.0))),
                }))),
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }
        );
    }

    #[test]
    fn test_complex_expression() {
        let tokens = "(3 + 5) * (10 - 2) / 4".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(
            ast,
            Expr::Binary {
                op: BinaryOperator::Div,
                left: Box::new(Expr::Binary {
                    op: BinaryOperator::Mul,
                    left: Box::new(Expr::Grouping(Box::new(Expr::Binary {
                        op: BinaryOperator::Add,
                        left: Box::new(Expr::Literal(Literal::Number(3.0))),
                        right: Box::new(Expr::Literal(Literal::Number(5.0))),
                    }))),
                    right: Box::new(Expr::Grouping(Box::new(Expr::Binary {
                        op: BinaryOperator::Sub,
                        left: Box::new(Expr::Literal(Literal::Number(10.0))),
                        right: Box::new(Expr::Literal(Literal::Number(2.0))),
                    }))),
                }),
                right: Box::new(Expr::Literal(Literal::Number(4.0))),
            }
        );
    }

    #[test]
    fn test_invalid_expression() {
        let tokens = "42 +".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_unexpected_token() {
        let tokens = "+ 42".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let result = parser.parse();

        assert!(result.is_err());
    }

    #[test]
    fn test_empty_input() {
        let tokens = "".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let result = parser.parse();

        assert!(result.is_err());
    }
}
