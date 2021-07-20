use std::fmt;

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[allow(unreachable_patterns)]
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(x) => write!(f, "{}", x),
            Value::Nil => write!(f, "nil"),
            _ => unreachable!(),
        }
    }
}
