use crate::chunk::Chunk;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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

    pub fn is_function(&self) -> bool {
        #[allow(unreachable_patterns)]
        match self {
            Value::Obj(box object) => match object {
                Object::Function(_) => true,
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

// This ended up being a poor choice, but any other
// way would have been a pain to implement and use
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Object {
    String(String),
    Function(Rc<RefCell<FunctionObject>>),
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct FunctionObject {
    pub(crate) arity: usize,
    pub(crate) chunk: Rc<RefCell<Chunk>>,
    pub(crate) name: Object,
}

impl FunctionObject {
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: Rc::new(RefCell::new(Chunk::new())),
            name: Object::String(String::new()),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[allow(unreachable_patterns)]
        match self {
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Function(fun) => write!(f, "{}", (*fun).borrow().name),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for FunctionObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Object::String(s) = &self.name {
            if s.len() > 0 {
                write!(f, "<fn {}>", s)
            } else {
                write!(f, "<script>")
            }
        } else {
            write!(f, "<fn <corrupted>>")
        }
    }
}
