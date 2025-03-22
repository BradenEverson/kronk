//! Interpretter

use std::{error::Error, fmt::Display};

use crate::parser::{BinaryOperator, Expr, Literal, UnaryOperator};

/// An AST interpretter
#[derive(Debug, Default, Clone)]
pub struct Interpretter {}

/// An error during execution
#[derive(Debug, Clone, Copy)]
pub struct RuntimeError;

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime Error Occured :(")
    }
}

impl Error for RuntimeError {}

impl Interpretter {
    /// Interprets an AST
    pub fn eval<'a>(&mut self, ast: &Expr<'a>) -> Result<Literal<'a>, RuntimeError> {
        match ast {
            Expr::Literal(l) => Ok(*l),
            Expr::Grouping(inner) => self.eval(inner),
            _ => todo!(),
        }
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{l}"),
            Self::Unary { op, node } => match op {
                UnaryOperator::Neg => write!(f, "-{node}"),
                UnaryOperator::Not => write!(f, "!{node}"),
            },
            Self::Binary { op, left, right } => match op {
                BinaryOperator::Add => write!(f, "{left} + {right}"),
                BinaryOperator::Sub => write!(f, "{left} - {right}"),
                BinaryOperator::Mul => write!(f, "{left} * {right}"),
                BinaryOperator::Div => write!(f, "{left} / {right}"),

                BinaryOperator::Eq => write!(f, "{left} == {right}"),
                BinaryOperator::Neq => write!(f, "{left} != {right}"),
                BinaryOperator::Gt => write!(f, "{left} > {right}"),
                BinaryOperator::Gte => write!(f, "{left} >= {right}"),
                BinaryOperator::Lt => write!(f, "{left} < {right}"),
                BinaryOperator::Lte => write!(f, "{left} <= {right}"),
            },
            Self::Grouping(e) => write!(f, "({e})"),
        }
    }
}
