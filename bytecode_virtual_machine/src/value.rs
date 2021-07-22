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
        if let Value::Obj(box Object::String(_)) = self {
            true
        } else {
            false
        }
    }

    pub fn is_function(&self) -> bool {
        if let Value::Obj(box Object::Function(_)) = self {
            true
        } else {
            false
        }
    }

    pub fn is_native(&self) -> bool {
        if let Value::Obj(box Object::Native(_)) = self {
            true
        } else {
            false
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
    Native(NativeFn),
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

#[derive(Clone)]
pub struct NativeFn(pub Rc<dyn Fn(usize, &[Value]) -> Value>);

impl std::cmp::PartialEq for NativeFn {
    fn eq(&self, _: &Self) -> bool {
        false
    }

    fn ne(&self, _: &Self) -> bool {
        false
    }
}

impl std::cmp::PartialOrd for NativeFn {
    fn partial_cmp(&self, _: &NativeFn) -> Option<std::cmp::Ordering> {
        None
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[allow(unreachable_patterns)]
        match self {
            Object::String(s) => write!(f, "{}", s),
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
