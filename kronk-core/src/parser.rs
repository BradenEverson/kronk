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
    pub fn peek(&self) -> Token<'a> {
        self.tokens[self.idx]
    }

    /// Consumes the current token assuming it's the provided Token, failing if not
    pub fn consume(&mut self, token: &Token<'_>) -> Result<(), ParseError> {
        if &self.peek() == token {
            self.idx += 1;
            Ok(())
        } else {
            Err(ParseError)
        }
    }

    /// Checks if stream is finished
    pub fn at_end(&self) -> bool {
        self.peek() == Token::EOF
    }

    /// Advances forward and returns the token we hop after
    pub fn advance(&mut self) -> Token<'_> {
        let curr = self.peek();
        if !self.at_end() {
            self.idx += 1
        }

        curr
    }
}
