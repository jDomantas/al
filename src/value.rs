use std::fmt;
use std::rc::Rc;
use num_bigint::BigInt;

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Nil,
    Symbol(u64, &'a str),
    Num(BigInt),
    Pair(Rc<Value<'a>>, Rc<Value<'a>>),
}

impl<'a> Value<'a> {
    fn fmt_list_tail(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tail = self;
        loop {
            match tail {
                Value::Nil => return write!(f, ")"),
                Value::Pair(car, cdr) => {
                    write!(f, " {}", car)?;
                    tail = cdr;
                }
                other => return write!(f, " . {})", other),
            }
        }
    }

    pub fn from_list(values: &[Value<'a>]) -> Self {
        let mut result = Value::Nil;
        for value in values.iter().rev() {
            result = Value::Pair(
                Rc::new(value.clone()),
                Rc::new(result),
            );
        }
        result
    }

    pub fn is_symbol(&self, name: &str) -> bool {
        if let Value::Symbol(_, x) = *self {
            x == name
        } else {
            false
        }
    }

    pub fn is_sym(&self) -> bool {
        if let Value::Symbol(_, _) = *self {
            true
        } else {
            false
        }
    }

    pub fn is_num(&self) -> bool {
        if let Value::Num(_) = *self {
            true
        } else {
            false
        }
    }

    pub fn is_pair(&self) -> bool {
        if let Value::Pair(_, _) = self {
            true
        } else {
            false
        }
    }

    pub fn car(&self) -> Option<Rc<Value<'a>>> {
        if let Value::Pair(car, _) = self {
            Some(car.clone())
        } else {
            None
        }
    }

    pub fn cdr(&self) -> Option<Rc<Value<'a>>> {
        if let Value::Pair(_, cdr) = self {
            Some(cdr.clone())
        } else {
            None
        }
    }

    pub fn truthy(&self) -> bool {
        if let Value::Nil = self {
            false
        } else {
            true
        }
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Symbol(_, s) => write!(f, "{}", s),
            Value::Num(n) => fmt::Display::fmt(n, f),
            Value::Nil => write!(f, "()"),
            Value::Pair(car, cdr) => {
                write!(f, "({}", car)?;
                cdr.fmt_list_tail(f)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_symbol() {
        let val = Value::Symbol(0, "foo");
        assert_eq!(&val.to_string(), "foo");
    }

    #[test]
    fn display_number() {
        let num = BigInt::from(123);
        let val = Value::Num(num);
        assert_eq!(&val.to_string(), "123");
    }

    #[test]
    fn display_nil() {
        let val = Value::Nil;
        assert_eq!(&val.to_string(), "()");
    }

    #[test]
    fn display_pair() {
        let car = Value::Symbol(0, "foo");
        let cdr = Value::Symbol(1, "bar");
        let val = Value::Pair(Rc::new(car), Rc::new(cdr));
        assert_eq!(&val.to_string(), "(foo . bar)");
    }

    #[test]
    fn display_list() {
        let val = Value::from_list(&[
            Value::Symbol(0, "a"),
            Value::Symbol(1, "b"),
            Value::Symbol(2, "c"),
            Value::Symbol(3, "d"),
        ]);
        assert_eq!(&val.to_string(), "(a b c d)");
    }

    #[test]
    fn display_singletop_list() {
        let val = Value::from_list(&[
            Value::Symbol(0, "a"),
        ]);
        assert_eq!(&val.to_string(), "(a)");
    }

    #[test]
    fn display_weird_list() {
        let val = Value::Pair(
            Rc::new(Value::Symbol(0, "a")),
            Rc::new(Value::Pair(
                Rc::new(Value::Symbol(1, "b")),
                Rc::new(Value::Symbol(2, "c")),
            )),
        );
        assert_eq!(&val.to_string(), "(a b . c)");
    }
}
