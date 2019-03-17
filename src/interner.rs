use std::collections::HashMap;
use std::marker::PhantomData;
use crate::value::Value;

#[derive(Debug, Default)]
pub struct Interner<'a> {
    next_symbol: u64,
    known_symbols: HashMap<&'static str, u64>,
    phantom: PhantomData<&'a str>,
}

impl<'a> Interner<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn intern(&mut self, string: String) -> (u64, &'a str) {
        // FIXME: eww
        let string = Box::leak(string.into_boxed_str());
        if let Some(&sym) = self.known_symbols.get(string) {
            (sym, string)
        } else {
            let sym = self.next_symbol;
            self.next_symbol += 1;
            self.known_symbols.insert(string, sym);
            (sym, string)
        }
    }

    pub fn symbol_value(&mut self, string: String) -> Value<'a> {
        let (sym, name) = self.intern(string);
        Value::Symbol(sym, name)
    }
}
