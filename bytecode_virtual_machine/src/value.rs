use std::fmt;

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Obj(Box<Object>),
    Nil,
}

impl Value {
    pub fn is_number(&self) -> bool {
        match self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Value::Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(b) => !*b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        #[allow(unreachable_patterns)]
        match self {
            Value::Obj(box object) => match object {
                Object::String(_) => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[allow(unreachable_patterns)]
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(x) => write!(f, "{}", x),
            Value::Nil => write!(f, "nil"),
            Value::Obj(box obj) => write!(f, "{}", obj),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Object {
    String(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[allow(unreachable_patterns)]
        match self {
            Object::String(s) => write!(f, "\"{}\"", s),
            _ => unreachable!(),
        }
    }
}
