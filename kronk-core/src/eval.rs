//! Interpretter

use std::{
    collections::HashMap,
    error::Error,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::parser::{BinaryOperator, Expr, Literal, UnaryOperator};

/// An AST interpretter
#[derive(Debug, Default, Clone)]
pub struct Interpretter<'a> {
    context: HashMap<String, Literal<'a>>,
}

/// An error during execution
#[derive(Debug, Clone, Copy)]
pub struct RuntimeError;

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Runtime Error Occured :(")
    }
}

impl Error for RuntimeError {}

impl<'a> Interpretter<'a> {
    /// Interprets an AST
    pub fn eval(&mut self, ast: Expr<'a>) -> Result<Literal<'a>, RuntimeError> {
        match ast {
            Expr::Print(node) => {
                println!("{}", self.eval(*node)?);
                Ok(Literal::Void)
            }
            Expr::Assignment { name, val } => {
                let val = self.eval(*val)?;
                self.context.insert(name.to_string(), val);
                Ok(Literal::Void)
            }
            Expr::Literal(l) => Ok(l),
            Expr::Grouping(inner) => self.eval(*inner),
            Expr::Binary { op, left, right } => op.eval(self.eval(*left)?, self.eval(*right)?),
            Expr::Unary { op, node } => op.eval(self.eval(*node)?),
        }
    }
}

impl BinaryOperator {
    /// Evaluates a binary operation
    pub fn eval<'a>(
        &self,
        left: Literal<'a>,
        right: Literal<'a>,
    ) -> Result<Literal<'a>, RuntimeError> {
        match self {
            Self::Add => left + right,
            Self::Sub => left - right,
            Self::Mul => left * right,
            Self::Div => left / right,
            Self::Eq => left.equals(&right),
            Self::Neq => left.not_equals(&right),

            op => todo!("Implement op {op:?}"),
        }
    }
}

impl UnaryOperator {
    /// Evaluates a unary operation
    pub fn eval<'a>(&self, node: Literal<'a>) -> Result<Literal<'a>, RuntimeError> {
        todo!()
    }
}

impl Add for Literal<'_> {
    type Output = Result<Self, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Literal::Number(n1 + n2)),

            (Self::String(s1), Self::String(s2)) => Ok(Literal::Concat(
                Box::new(Literal::String(s1)),
                Box::new(Literal::String(s2)),
            )),

            (Self::Number(n1), Self::String(s2)) => Ok(Literal::Concat(
                Box::new(Literal::Number(n1)),
                Box::new(Literal::String(s2)),
            )),

            (Self::String(s1), Self::Number(n2)) => Ok(Literal::Concat(
                Box::new(Literal::String(s1)),
                Box::new(Literal::Number(n2)),
            )),
            _ => Err(RuntimeError),
        }
    }
}

impl Sub for Literal<'_> {
    type Output = Result<Self, RuntimeError>;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 - n2)),

            _ => Err(RuntimeError),
        }
    }
}

impl Mul for Literal<'_> {
    type Output = Result<Self, RuntimeError>;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 * n2)),

            _ => Err(RuntimeError),
        }
    }
}

impl Div for Literal<'_> {
    type Output = Result<Self, RuntimeError>;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Number(n1), Self::Number(n2)) => Ok(Self::Number(n1 / n2)),

            _ => Err(RuntimeError),
        }
    }
}

impl<'a> Literal<'a> {
    /// Returns the True literal if the two literals are losely equal
    pub fn equals(&self, other: &Self) -> Result<Literal<'a>, RuntimeError> {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => {
                Ok(if n1 == n2 { Self::True } else { Self::False })
            }
            (Self::True, Self::True) => Ok(Self::True),
            (Self::False, Self::False) => Ok(Self::True),

            (Self::False, Self::True) => Ok(Self::False),
            (Self::True, Self::False) => Ok(Self::False),

            (Self::Void, Self::Void) => Ok(Self::True),

            (crazy1, crazy2) if crazy1.to_string() == crazy2.to_string() => Ok(Self::True),
            (crazy1, crazy2) if crazy1.to_string() != crazy2.to_string() => Ok(Self::False),

            _ => Err(RuntimeError),
        }
    }

    /// Returns the True literal if the two literals are not losely equal
    pub fn not_equals(&self, other: &Self) -> Result<Literal<'a>, RuntimeError> {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => {
                Ok(if n1 == n2 { Self::False } else { Self::True })
            }
            (Self::True, Self::True) => Ok(Self::False),
            (Self::False, Self::False) => Ok(Self::False),

            (Self::False, Self::True) => Ok(Self::True),
            (Self::True, Self::False) => Ok(Self::True),

            (Self::Void, Self::Void) => Ok(Self::False),

            (crazy1, crazy2) if crazy1.to_string() == crazy2.to_string() => Ok(Self::False),
            (crazy1, crazy2) if crazy1.to_string() != crazy2.to_string() => Ok(Self::True),

            _ => Err(RuntimeError),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Print(node) => write!(f, "print {node}"),
            Self::Assignment { name, val } => write!(f, "{name} = {val}"),
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

#[cfg(test)]
mod tests {
    use crate::{
        parser::{Literal, Parser},
        tokenizer::Tokenizable,
    };

    use super::Interpretter;

    #[test]
    fn eval_addition() {
        let tokens = "100 + 100".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();

        assert_eq!(eval.eval(ast).expect("Evaluate"), Literal::Number(200.0))
    }

    #[test]
    fn eval_string_concat() {
        let tokens = r#""100" + 100"#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        let result = eval.eval(ast).expect("Eval").to_string();

        assert_eq!(result, "100100");
    }

    #[test]
    fn eval_string_concat_otherway() {
        let tokens = r#"10 + "20""#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        let result = eval.eval(ast).expect("Eval").to_string();

        assert_eq!(result, "1020");
    }

    #[test]
    fn loose_equality() {
        let tokens = r#"1 == "1""#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        assert_eq!(eval.eval(ast).expect("Eval"), Literal::True)
    }

    #[test]
    fn pure_inequality() {
        let tokens = "1 + 1 != 100 * 10".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        assert_eq!(eval.eval(ast).expect("Eval"), Literal::True)
    }

    #[test]
    fn pure_equality() {
        let tokens = "1 + 1 == 2".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        assert_eq!(eval.eval(ast).expect("Eval"), Literal::True)
    }

    #[test]
    fn eval_pure_string_concat() {
        let tokens = r#""Hello " + "World!""#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        let result = eval.eval(ast).expect("Eval").to_string();

        assert_eq!(result, "Hello World!");
    }
}
