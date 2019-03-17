use std::rc::Rc;
use num_bigint::BigInt;
use crate::value::Value;
use crate::interner::Interner;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum ErrorKind {
    UnknownChar,
    UnexpectedEof,
    UnexpectedToken,
    BadNumber,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub offset: usize,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(PartialEq, Eq, Debug)]
enum Token<'a> {
    LeftParen,
    RightParen,
    Ident(&'a str),
    Number(BigInt),
    Dot,
    Quote,
    QuasiQuote,
    UnQuote,
}

fn is_symbol_char(c: char) -> bool {
    match c {
        '!' |
        '#' ..= '&' |
        '*' |
        '+' |
        '-' |
        '/' ..= 'z' |
        '|' |
        '~' => true,
        _ => false,
    }
}

struct Parser<'a, 'src, 'sym> {
    src: &'src str,
    interner: &'a mut Interner<'sym>,
    quote: Value<'sym>,
    quasi_quote: Value<'sym>,
    unquote: Value<'sym>,
    offset: usize,
}

impl<'a, 'src, 'sym> Parser<'a, 'src, 'sym> {
    fn new(src: &'src str, interner: &'a mut Interner<'sym>) -> Self {
        let quote = interner.symbol_value(String::from("quote"));
        let quasi_quote = interner.symbol_value(String::from("quasi-quote"));
        let unquote = interner.symbol_value(String::from("unquote"));
        Parser {
            src,
            interner,
            quote,
            quasi_quote,
            unquote,
            offset: 0,
        }
    }

    fn collect_indent(&mut self) -> &'src str {
        let mut chars = self.src.chars();
        loop {
            let len = chars.as_str().len();
            match chars.next() {
                Some(ch) if is_symbol_char(ch) => {}
                _ => {
                    let len = self.src.len() - len;
                    let ident = &self.src[..len];
                    self.src = &self.src[len..];
                    self.offset += len;
                    return ident;
                }
            }
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let mut chars = self.src.chars();
            let first = chars.next();
            let remaining = chars.as_str();
            match first {
                Some(' ') |
                Some('\n') |
                Some('\r') |
                Some('\t') => {
                    self.offset += 1;
                    self.src = remaining;
                }
                _ => break,
            }
        }
    }

    fn token(&mut self) -> Result<Option<Token<'src>>> {
        self.skip_whitespace();
        let mut chars = self.src.chars();
        let first = chars.next();
        let remaining = chars.as_str();
        match first {
            None => Ok(None),
            Some('\'') => {
                self.offset += 1;
                self.src = remaining;
                Ok(Some(Token::Quote))
            }
            Some('`') => {
                self.offset += 1;
                self.src = remaining;
                Ok(Some(Token::QuasiQuote))
            }
            Some(',') => {
                self.offset += 1;
                self.src = remaining;
                Ok(Some(Token::UnQuote))
            }
            Some('(') => {
                self.offset += 1;
                self.src = remaining;
                Ok(Some(Token::LeftParen))
            }
            Some(')') => {
                self.offset += 1;
                self.src = remaining;
                Ok(Some(Token::RightParen))
            }
            Some('.') => {
                self.offset += 1;
                self.src = remaining;
                Ok(Some(Token::Dot))
            }
            Some('0' ..= '9') => {
                let offset = self.offset;
                let num = self.collect_indent();
                match num.parse::<BigInt>() {
                    Ok(num) => Ok(Some(Token::Number(num))),
                    Err(_) => Err(Error {
                        offset,
                        kind: ErrorKind::BadNumber,
                    }),
                }
            }
            Some(ch) if is_symbol_char(ch) => {
                let ident = self.collect_indent();
                Ok(Some(Token::Ident(ident)))
            }
            _ => {
                Err(Error {
                    offset: self.offset,
                    kind: ErrorKind::UnknownChar,
                })
            }
        }
    }

    fn parse_value(&mut self) -> Result<Option<Value<'sym>>> {
        self.skip_whitespace();
        let offset = self.offset;
        match self.token()? {
            None => Ok(None),
            Some(Token::Ident(ident)) => {
                let (sym, name) = self.interner.intern(String::from(ident));
                Ok(Some(Value::Symbol(sym, name)))
            }
            Some(Token::Number(num)) => {
                Ok(Some(Value::Num(num)))
            }
            Some(Token::Dot) |
            Some(Token::RightParen) => {
                Err(Error {
                    offset,
                    kind: ErrorKind::UnexpectedToken,
                })
            }
            Some(Token::Quote) => {
                let arg = self.parse_some_value()?;
                Ok(Some(Value::from_list(&[
                    self.quote.clone(),
                    arg,
                ])))
            }
            Some(Token::QuasiQuote) => {
                let arg = self.parse_some_value()?;
                Ok(Some(Value::from_list(&[
                    self.quasi_quote.clone(),
                    arg,
                ])))
            }
            Some(Token::UnQuote) => {
                let arg = self.parse_some_value()?;
                Ok(Some(Value::from_list(&[
                    self.unquote.clone(),
                    arg,
                ])))
            }
            Some(Token::LeftParen) => {
                Ok(Some(self.parse_list_tail()?))
            }
        }
    }

    fn parse_list_tail(&mut self) -> Result<Value<'sym>> {
        self.skip_whitespace();
        let offset = self.offset;
        let src = self.src;
        match self.token()? {
            None => Err(Error {
                offset,
                kind: ErrorKind::UnexpectedEof,
            }),
            Some(Token::RightParen) => Ok(Value::Nil),
            Some(Token::Dot) => {
                let cdr = self.parse_some_value()?;
                self.expect_right_paren()?;
                Ok(cdr)
            }
            _ => {
                self.src = src;
                self.offset = offset;
                let item = self.parse_some_value()?;
                let tail = self.parse_list_tail()?;
                Ok(Value::Pair(
                    Rc::new(item),
                    Rc::new(tail),
                ))
            }
        }
    }

    fn expect_right_paren(&mut self) -> Result<()> {
        self.skip_whitespace();
        let offset = self.offset;
        match self.token()? {
            Some(Token::RightParen) => Ok(()),
            None => Err(Error {
                offset,
                kind: ErrorKind::UnexpectedEof,
            }),
            _ => Err(Error {
                offset,
                kind: ErrorKind::UnexpectedToken,
            }),
        }
    }

    fn parse_some_value(&mut self) -> Result<Value<'sym>> {
        self.skip_whitespace();
        let offset = self.offset;
        match self.parse_value()? {
            Some(val) => Ok(val),
            None => Err(Error {
                offset,
                kind: ErrorKind::UnexpectedEof,
            })
        }
    }
}

pub fn parse<'a>(source: &str, interner: &mut Interner<'a>) -> Result<Value<'a>> {
    let mut parser = Parser::new(source, interner);
    let value = parser.parse_some_value()?;
    parser.skip_whitespace();
    let offset = parser.offset;
    if parser.token()?.is_none() {
        Ok(value)
    } else {
        Err(Error {
            offset,
            kind: ErrorKind::UnexpectedToken,
        })
    }
}

#[allow(unused)]
pub fn parse_all<'a>(source: &str, interner: &mut Interner<'a>) -> Result<Vec<Value<'a>>> {
    let mut parser = Parser::new(source, interner);
    let mut result = Vec::new();
    while let Some(value) = parser.parse_value()? {
        result.push(value);
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip() {
        let src = "(a b 1 (+ foo-bar) () (a . b) (a b . c))";
        let mut interner = Interner::new();
        let value = parse(src, &mut interner).unwrap();
        assert_eq!(&value.to_string(), src);
    }

    #[test]
    fn quoting() {
        let src = "('a `a ,a '''a)";
        let expected = "((quote a) (quasi-quote a) (unquote a) (quote (quote (quote a))))";
        let mut interner = Interner::new();
        let value = parse(src, &mut interner).unwrap();
        assert_eq!(&value.to_string(), expected);
    }
}
