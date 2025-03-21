//! Interpretter

use std::fmt::Display;

use crate::parser::{BinaryOperator, Expr, Literal, UnaryOperator};

/// An AST interpretter
#[derive(Debug, Default, Clone)]
pub struct Interpretter {}

impl Interpretter {
    /// Interprets an AST
    pub fn eval(&mut self, ast: Expr<'_>) {}
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

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "{s}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Nil => write!(f, "nil"),
        }
    }
}
