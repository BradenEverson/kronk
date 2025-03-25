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
    /// Local variables
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
            Expr::Index { item, index } => {
                let idx = self.eval(*index)?.uint()?;
                let item = self.eval(*item)?;

                match item {
                    Literal::String(s) => Ok(Literal::String(&s[idx..=idx])),
                    Literal::List(l) => Ok(l[idx].clone()),
                    _ => Err(RuntimeError),
                }
            }
            Expr::List(items) => {
                let mut literals = vec![];

                for item in items {
                    literals.push(self.eval(item)?);
                }

                Ok(Literal::List(literals))
            }
            Expr::Inc(name, before) => {
                if let Some(inc) = self.context.get_mut(name) {
                    let prev = inc.clone();
                    *inc = Literal::Number(inc.number()? + 1.0);

                    if before { Ok(prev) } else { Ok(inc.clone()) }
                } else {
                    // TODO: Make this a more descriptive "variable does not exist" error
                    Err(RuntimeError)
                }
            }
            Expr::AddAssign { name, add } => {
                let val = self.eval(*add)?;
                if let Some(inc) = self.context.get_mut(name) {
                    *inc = (inc.clone() + val)?;

                    Ok(Literal::Void)
                } else {
                    // TODO: Make this a more descriptive "variable does not exist" error
                    Err(RuntimeError)
                }
            }
            Expr::Reassignment { name, val } => {
                let val = self.eval(*val)?;
                if let Some(set) = self.context.get_mut(name) {
                    *set = val;
                    Ok(Literal::Void)
                } else {
                    // TODO: Make this a more descriptive "variable does not exist" error
                    Err(RuntimeError)
                }
            }
            Expr::Roar(expr) => {
                let mut stringified = format!("{}", self.eval(*expr)?);
                stringified = stringified.to_uppercase();
                println!("{stringified}!!!");

                Ok(Literal::Void)
            }
            Expr::ForLoop {
                init,
                check,
                update,
                exec,
            } => {
                self.eval(*init)?;
                while self.eval(*check.clone())?.bool()? {
                    self.eval(*exec.clone())?;
                    self.eval(*update.clone())?;
                }

                Ok(Literal::Void)
            }

            Expr::WhileLoop { condition, exec } => {
                while self.eval(*condition.clone())?.bool()? {
                    self.eval(*exec.clone())?;
                }

                Ok(Literal::Void)
            }
            Expr::Conditional {
                condition,
                true_branch,
                else_branch,
            } => {
                let cond_check = self.eval(*condition)?.bool()?;

                if cond_check {
                    self.eval(*true_branch)
                } else if let Some(elb) = else_branch {
                    self.eval(*elb)
                } else {
                    Ok(Literal::Void)
                }
            }

            Expr::Block(exprs) => {
                for expr in exprs {
                    self.eval(expr)?;
                }

                Ok(Literal::Void)
            }
            Expr::Variable(var) => Ok(self.context[var].clone()),
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

            Self::Gt => Ok((left.number()? > right.number()?).into()),
            Self::Gte => Ok((left.number()? >= right.number()?).into()),
            Self::Lt => Ok((left.number()? < right.number()?).into()),
            Self::Lte => Ok((left.number()? <= right.number()?).into()),

            Self::Eq => left.equals(&right),
            Self::Neq => left.not_equals(&right),
        }
    }
}

impl UnaryOperator {
    /// Evaluates a unary operation
    pub fn eval<'a>(&self, node: Literal<'a>) -> Result<Literal<'a>, RuntimeError> {
        match self {
            Self::Neg => Ok(Literal::Number(-node.number()?)),
            Self::Not => Ok(Literal::from(!node.bool()?)),
        }
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

impl From<bool> for Literal<'_> {
    fn from(value: bool) -> Self {
        if value { Literal::True } else { Literal::False }
    }
}

impl<'a> Literal<'a> {
    /// Returns the inner literal if it's a proper unsigned integer truly, asserting a runtime
    /// error if not
    pub fn uint(&self) -> Result<usize, RuntimeError> {
        match self {
            Self::Number(n) if *n >= 0.0 && n.round() == *n => Ok(*n as usize),
            _ => Err(RuntimeError),
        }
    }
    /// Returns the inner literal if it's numeric, asserting a runtime error if not
    pub fn number(&self) -> Result<f64, RuntimeError> {
        match self {
            Self::Number(n) => Ok(*n),
            _ => Err(RuntimeError),
        }
    }

    /// Returns the inner literal if it's boolean, asserting a runtime error if not
    pub fn bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Self::True => Ok(true),
            Self::False => Ok(false),
            Self::Number(0.0) => Ok(false),
            Self::Number(_) => Ok(true),
            _ => Err(RuntimeError),
        }
    }

    /// Returns the True literal if the two literals are losely equal
    pub fn equals(&self, other: &Self) -> Result<Literal<'a>, RuntimeError> {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => Ok((n1 == n2).into()),
            (Self::True, Self::True) => Ok(Self::True),
            (Self::False, Self::False) => Ok(Self::True),

            (Self::False, Self::True) => Ok(Self::False),
            (Self::True, Self::False) => Ok(Self::False),

            (Self::Void, Self::Void) => Ok(Self::True),

            (crazy1, crazy2) => Ok((crazy1.to_string() == crazy2.to_string()).into()),
        }
    }

    /// Returns the True literal if the two literals are not losely equal
    pub fn not_equals(&self, other: &Self) -> Result<Literal<'a>, RuntimeError> {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => Ok((n1 != n2).into()),
            (Self::True, Self::True) => Ok(Self::False),
            (Self::False, Self::False) => Ok(Self::False),

            (Self::False, Self::True) => Ok(Self::True),
            (Self::True, Self::False) => Ok(Self::True),

            (Self::Void, Self::Void) => Ok(Self::False),

            (crazy1, crazy2) => Ok((crazy1.to_string() != crazy2.to_string()).into()),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Index { item, index } => write!(f, "{item}[{index}]"),
            Expr::List(items) => write!(f, "{items:?}"),
            Expr::AddAssign { name, add } => write!(f, "{name} += {add}"),
            Expr::Inc(name, before) => {
                if *before {
                    write!(f, "++{name}")
                } else {
                    write!(f, "{name}++")
                }
            }
            Expr::Roar(r) => write!(f, "roar {r}!"),
            Expr::ForLoop {
                init,
                check,
                update,
                exec,
            } => {
                writeln!(f, "for ({init}; {check}; {update}) {exec}")
            }

            Expr::WhileLoop { condition, exec } => {
                writeln!(f, "while ({condition}) {{\n\t{exec}\n}}")
            }

            Expr::Conditional {
                condition,
                true_branch,
                else_branch,
            } => {
                if let Some(elb) = else_branch {
                    writeln!(
                        f,
                        "if ({}) {{\n\t{}\n}} else {{\n\t{}\n}}",
                        condition, true_branch, elb
                    )
                } else {
                    writeln!(f, "if ({}) {{\n\t{}\n}}", condition, true_branch)
                }
            }
            Self::Block(b) => {
                writeln!(f, "{{")?;

                for expr in b {
                    writeln!(f, "\t{expr}")?;
                }

                write!(f, "}}")
            }
            Self::Variable(v) => write!(f, "{v}"),
            Self::Print(node) => write!(f, "print {node}"),
            Self::Assignment { name, val } => write!(f, "var {name} = {val}"),
            Self::Reassignment { name, val } => write!(f, "{name} = {val}"),
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
    fn for_looping() {
        let tokens = r#"
            var foo = 10;
            for (var i = 0; i < 10; var i = i + 1) {
                var foo = foo + 1;
            }
            foo;
            "#
        .tokenize()
        .expect("Tokenize");

        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse_many().expect("Failed to parse");
        let mut interp = Interpretter::default();

        let mut val = None;
        for expr in ast {
            val = Some(interp.eval(expr).expect("Interpret result"));
        }

        assert_eq!(val.unwrap(), Literal::Number(20.0))
    }

    #[test]
    fn use_variables_later() {
        let tokens = "var foo = 100;".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut interp = Interpretter::default();
        interp.eval(ast).expect("Interpret result");

        let tokens = "foo + 1;".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");

        assert_eq!(interp.eval(ast).expect("Eval"), Literal::Number(101.0))
    }

    #[test]
    fn simple_eval() {
        let tokens = "var foo = 100;".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut interp = Interpretter::default();
        interp.eval(ast).expect("Interpret result");

        assert_eq!(interp.context["foo"], Literal::Number(100.0))
    }

    #[test]
    fn notting() {
        let tokens = "!true;".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut interp = Interpretter::default();
        let res = interp.eval(ast).expect("Interpret result");
        assert_eq!(res, Literal::False)
    }

    #[test]
    fn negation() {
        let tokens = "-100;".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut interp = Interpretter::default();
        let res = interp.eval(ast).expect("Interpret result");
        assert_eq!(res, Literal::Number(-100.0))
    }

    #[test]
    fn eval_addition() {
        let tokens = "100 + 100;".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();

        assert_eq!(eval.eval(ast).expect("Evaluate"), Literal::Number(200.0))
    }

    #[test]
    fn eval_string_concat() {
        let tokens = r#""100" + 100;"#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        let result = eval.eval(ast).expect("Eval").to_string();

        assert_eq!(result, "100100");
    }

    #[test]
    fn eval_string_concat_otherway() {
        let tokens = r#"10 + "20";"#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        let result = eval.eval(ast).expect("Eval").to_string();

        assert_eq!(result, "1020");
    }

    #[test]
    fn loose_equality() {
        let tokens = r#"1 == "1";"#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        assert_eq!(eval.eval(ast).expect("Eval"), Literal::True)
    }

    #[test]
    fn pure_inequality() {
        let tokens = "1 + 1 != 100 * 10;".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        assert_eq!(eval.eval(ast).expect("Eval"), Literal::True)
    }

    #[test]
    fn list() {
        let tokens = r#"[12 == 1, 10 * 10, "hi"];"#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        assert_eq!(
            eval.eval(ast).expect("Eval"),
            Literal::List(vec![
                Literal::False,
                Literal::Number(100.0),
                Literal::String("hi")
            ])
        )
    }

    #[test]
    fn pure_equality() {
        let tokens = "1 + 1 == 2;".tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        assert_eq!(eval.eval(ast).expect("Eval"), Literal::True)
    }

    #[test]
    fn eval_pure_string_concat() {
        let tokens = r#""Hello " + "World!";"#.tokenize().expect("Tokenize");
        let mut parser = Parser::with_tokens(&tokens);

        let ast = parser.parse().expect("Failed to parse");
        let mut eval = Interpretter::default();
        let result = eval.eval(ast).expect("Eval").to_string();

        assert_eq!(result, "Hello World!");
    }
}
