use std::fmt;
use std::rc::Rc;
use crate::value::Value;
use crate::interner::Interner;

#[derive(Debug, Clone)]
pub enum Error<'a> {
    Stuck(Rc<Value<'a>>),
    BadEnv(Rc<Value<'a>>),
    UnboundSym(&'a str),
    NotNumber(Rc<Value<'a>>),
    NotSymbol(Rc<Value<'a>>),
    NotPair(Rc<Value<'a>>),
    CannotCall(Rc<Value<'a>>),
    DivByZero,
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Stuck(term) => write!(f, "stuck on term: {}", term),
            Error::BadEnv(env) => write!(f, "bad env: {}", env),
            Error::UnboundSym(name) => write!(f, "unbound symbol: {}", name),
            Error::NotNumber(val) => write!(f, "not a number: {}", val),
            Error::NotSymbol(val) => write!(f, "not a symbol: {}", val),
            Error::NotPair(val) => write!(f, "not a pair: {}", val),
            Error::CannotCall(val) => write!(f, "cannot call: {}", val),
            Error::DivByZero => write!(f, "division by zero"),
        }
    }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

pub struct Eval<'a> {
    global_env: Rc<Value<'a>>,
    true_sym: Rc<Value<'a>>,
    nil: Rc<Value<'a>>,
    args_sym: Rc<Value<'a>>,
    call_env_sym: Rc<Value<'a>>,
    captured_env_sym: Rc<Value<'a>>,
    closure_sym: Rc<Value<'a>>,
}

impl<'a> Eval<'a> {
    pub fn new(interner: &mut Interner<'a>) -> Self {
        Eval {
            global_env: Rc::new(Value::Nil),
            true_sym: Rc::new(interner.symbol_value("t".into())),
            nil: Rc::new(Value::Nil),
            args_sym: Rc::new(interner.symbol_value("args".into())),
            call_env_sym: Rc::new(interner.symbol_value("call-env".into())),
            captured_env_sym: Rc::new(interner.symbol_value("captured-env".into())),
            closure_sym: Rc::new(interner.symbol_value("##closure".into())),
        }
    }

    pub fn eval_global(&mut self, value: Rc<Value<'a>>) -> Result<'a, Rc<Value<'a>>> {
        self.eval(value, self.global_env.clone())
    }

    pub fn eval(&mut self, value: Rc<Value<'a>>, env: Rc<Value<'a>>) -> Result<'a, Rc<Value<'a>>> {
        match &*value {
            Value::Num(_) |
            Value::Nil => Ok(value),
            Value::Symbol(id, name) => lookup(env, *id, *name),
            Value::Pair(car, cdr) if car.is_symbol("##if") => {
                let (c, t, e) = match unlist_three(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let c = self.eval(c, env.clone())?;
                if c.truthy() {
                    self.eval(t, env)
                } else {
                    self.eval(e, env)
                }
            }
            Value::Pair(car, cdr) if car.is_symbol("##macro") => {
                let body = match unlist_one(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                Ok(Rc::new(Value::Pair(
                    self.closure_sym.clone(),
                    Rc::new(Value::Pair(
                        body,
                        Rc::new(Value::Pair(
                            env,
                            Rc::new(Value::Nil),
                        )),
                    )),
                )))
            }
            Value::Pair(car, cdr) if car.is_symbol("##car") => {
                let arg = match unlist_one(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let arg = self.eval(arg, env)?;
                if let Some(val) = arg.car() {
                    Ok(val)
                } else {
                    Err(Error::NotPair(arg))
                }
            }
            Value::Pair(car, cdr) if car.is_symbol("##cdr") => {
                let arg = match unlist_one(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let arg = self.eval(arg, env)?;
                if let Some(val) = arg.cdr() {
                    Ok(val)
                } else {
                    Err(Error::NotPair(arg))
                }
            }
            Value::Pair(car, cdr) if car.is_symbol("##cons") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                Ok(Rc::new(Value::Pair(a, b)))
            }
            Value::Pair(car, cdr) if car.is_symbol("##eval") => {
                let (expr, env_arg) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let expr = self.eval(expr, env.clone())?;
                let env_arg = self.eval(env_arg, env.clone())?;
                self.eval(expr, env_arg)
            }
            Value::Pair(car, cdr) if car.is_symbol("##symbol?") => {
                let arg = match unlist_one(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let arg = self.eval(arg, env)?;
                Ok(self.from_bool(arg.is_sym()))
            }
            Value::Pair(car, cdr) if car.is_symbol("##number?") => {
                let arg = match unlist_one(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let arg = self.eval(arg, env)?;
                Ok(self.from_bool(arg.is_num()))
            }
            Value::Pair(car, cdr) if car.is_symbol("##pair?") => {
                let arg = match unlist_one(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let arg = self.eval(arg, env)?;
                Ok(self.from_bool(arg.is_pair()))
            }
            Value::Pair(car, cdr) if car.is_symbol("##eq-symbol?") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                let a = if let Value::Symbol(a, _) = *a {
                    a
                } else {
                    return Err(Error::NotSymbol(a));
                };
                let b = if let Value::Symbol(b, _) = *b {
                    b
                } else {
                    return Err(Error::NotSymbol(b));
                };
                Ok(self.from_bool(a == b))
            }
            Value::Pair(car, cdr) if car.is_symbol("##eq-number?") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                let a = if let Value::Num(ref a) = *a {
                    a
                } else {
                    return Err(Error::NotNumber(a));
                };
                let b = if let Value::Num(ref b) = *b {
                    b
                } else {
                    return Err(Error::NotNumber(b));
                };
                Ok(self.from_bool(a == b))
            }
            Value::Pair(car, cdr) if car.is_symbol("##eq-number?") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                let a = if let Value::Num(ref a) = *a {
                    a
                } else {
                    return Err(Error::NotNumber(a));
                };
                let b = if let Value::Num(ref b) = *b {
                    b
                } else {
                    return Err(Error::NotNumber(b));
                };
                Ok(self.from_bool(a == b))
            }
            Value::Pair(car, cdr) if car.is_symbol("##+") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                let a = if let Value::Num(ref a) = *a {
                    a
                } else {
                    return Err(Error::NotNumber(a));
                };
                let b = if let Value::Num(ref b) = *b {
                    b
                } else {
                    return Err(Error::NotNumber(b));
                };
                Ok(Rc::new(Value::Num(a + b)))
            }
            Value::Pair(car, cdr) if car.is_symbol("##-") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                let a = if let Value::Num(ref a) = *a {
                    a
                } else {
                    return Err(Error::NotNumber(a));
                };
                let b = if let Value::Num(ref b) = *b {
                    b
                } else {
                    return Err(Error::NotNumber(b));
                };
                Ok(Rc::new(Value::Num(a - b)))
            }
            Value::Pair(car, cdr) if car.is_symbol("##*") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                let a = if let Value::Num(ref a) = *a {
                    a
                } else {
                    return Err(Error::NotNumber(a));
                };
                let b = if let Value::Num(ref b) = *b {
                    b
                } else {
                    return Err(Error::NotNumber(b));
                };
                Ok(Rc::new(Value::Num(a * b)))
            }
            Value::Pair(car, cdr) if car.is_symbol("##/") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                let a = if let Value::Num(ref a) = *a {
                    a
                } else {
                    return Err(Error::NotNumber(a));
                };
                let b = if let Value::Num(ref b) = *b {
                    b
                } else {
                    return Err(Error::NotNumber(b));
                };
                if let Some(result) = a.checked_div(b) {
                    Ok(Rc::new(Value::Num(result)))
                } else {
                    Err(Error::DivByZero)
                }
            }
            Value::Pair(car, cdr) if car.is_symbol("##<") => {
                let (a, b) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let a = self.eval(a, env.clone())?;
                let b = self.eval(b, env)?;
                let a = if let Value::Num(ref a) = *a {
                    a
                } else {
                    return Err(Error::NotNumber(a));
                };
                let b = if let Value::Num(ref b) = *b {
                    b
                } else {
                    return Err(Error::NotNumber(b));
                };
                Ok(self.from_bool(a < b))
            }
            Value::Pair(car, cdr) if car.is_symbol("##def") => {
                let (sym, val) = match unlist_two(cdr) {
                    Some(x) => x,
                    None => return Err(Error::Stuck(value)),
                };
                let val = self.eval(val, env)?;
                if !sym.is_sym() {
                    return Err(Error::NotSymbol(sym.clone()));
                }
                self.global_env = add(self.global_env.clone(), sym, val);
                Ok(self.nil.clone())
            }
            Value::Pair(car, cdr) => {
                if !valid_list(cdr) {
                    return Err(Error::Stuck(value));
                }
                let f = self.eval(car.clone(), env.clone())?;
                let (m, b, e) = match unlist_three(&f) {
                    Some(x) => x,
                    None => return Err(Error::CannotCall(f)),
                };
                if !m.is_symbol("##closure") {
                    return Err(Error::CannotCall(f));
                }
                let ce = add(e.clone(), self.call_env_sym.clone(), env);
                let ce = add(ce, self.captured_env_sym.clone(), e);
                let ce = add(ce, self.args_sym.clone(), cdr.clone());
                self.eval(b, ce)
            }
        }
    }
    
    fn from_bool(&self, b: bool) -> Rc<Value<'a>> {
        if b {
            self.true_sym.clone()
        } else {
            self.nil.clone()
        }
    }
}

fn valid_list(mut list: &Value<'_>) -> bool {
    loop {
        match list {
            Value::Nil => return true,
            Value::Pair(_, cdr) => list = cdr,
            _ => return false,
        }
    }
}

fn unlist_one<'a>(val: &Value<'a>) -> Option<Rc<Value<'a>>> {
    if let Value::Pair(a1, r) = val {
        if let Value::Nil = &**r {
            return Some(a1.clone());
        }
    }
    None
}

fn unlist_two<'a>(val: &Value<'a>) -> Option<(Rc<Value<'a>>, Rc<Value<'a>>)> {
    if let Value::Pair(a1, r) = val {
        if let Value::Pair(a2, r) = &**r {
            if let Value::Nil = &**r {
                return Some((a1.clone(), a2.clone()));
            }
        }
    }
    None
}

fn unlist_three<'a>(val: &Value<'a>) -> Option<(Rc<Value<'a>>, Rc<Value<'a>>, Rc<Value<'a>>)> {
    if let Value::Pair(a1, r) = val {
        if let Value::Pair(a2, r) = &**r {
            if let Value::Pair(a3, r) = &**r {
                if let Value::Nil = &**r {
                    return Some((a1.clone(), a2.clone(), a3.clone()));
                }
            }
        }
    }
    None
}

fn lookup<'a>(env: Rc<Value<'a>>, sym: u64, name: &'a str) -> Result<'a, Rc<Value<'a>>> {
    let mut walk_env = &env;
    loop {
        match **walk_env {
            Value::Nil => return Err(Error::UnboundSym(name)),
            Value::Pair(ref car, ref rest) => {
                if let Value::Pair(ref car, ref cdr) = **car {
                    if let Value::Symbol(s, _) = **car {
                        if s == sym {
                            return Ok((*cdr).clone());
                        }
                    }
                    walk_env = rest;
                } else {
                    return Err(Error::BadEnv(env));
                }
            }
            _ => return Err(Error::BadEnv(env)),
        }
    }
}

fn add<'a>(env: Rc<Value<'a>>, sym: Rc<Value<'a>>, val: Rc<Value<'a>>) -> Rc<Value<'a>> {
    Rc::new(Value::Pair(
        Rc::new(Value::Pair(
            sym,
            val,
        )),
        env,
    ))
}
