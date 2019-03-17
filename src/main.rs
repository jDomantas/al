#![warn(rust_2018_idioms)]
#![forbid(unsafe_code)]

use std::io::Write;
use std::rc::Rc;

mod value;
mod interner;
mod parser;
mod eval;

const BUILTINS: &[&str] = &[
    "(##def quote (##macro (##car args)))",
    "(##def car (##macro (##car (##eval (##car args) call-env))))",
    "(##def cdr (##macro (##cdr (##eval (##car args) call-env))))",
    "(##def cadr (##macro (car (cdr (##eval (car args) call-env)))))",
    "(##def caddr (##macro (car (cdr (cdr (##eval (car args) call-env))))))",
    "(##def cadddr (##macro (car (cdr (cdr (cdr (##eval (car args) call-env)))))))",
    "(##def cons (##macro (##cons (##eval (car args) call-env) (##eval (cadr args) call-env))))",
    "(##def let (##macro (##eval (caddr args) (cons (cons (car args) (##eval (cadr args) call-env)) call-env))))",
    "(##def if (##macro (##if (##eval (car args) call-env) (##eval (cadr args) call-env) (##eval (caddr args) call-env))))",
    "(##def eval-all-rec (##macro
        (let self (##eval (car args) call-env)
        (let terms (##eval (cadr args) call-env)
        (let call-env (##eval (caddr args) call-env)
            (if terms
                (cons
                    (##eval (car terms) call-env)
                    (self self (cdr terms) call-env))
                ()))))))",
    "(##def eval-all (##macro
        (let terms (##eval (car args) call-env)
        (let call-env (##eval (cadr args) call-env)
            (eval-all-rec eval-all-rec terms call-env)))))",
    "(##def symbol? (##macro (##symbol? (##eval (##car args) call-env))))",
    "(##def pair? (##macro (##pair? (##eval (##car args) call-env))))",
    "(##def nil? (##macro (if (##eval (##car args) call-env) () 't)))",
    "(##def list (##macro (eval-all args call-env)))",
    "(##def strict-and-pair (##macro (let a (##eval (car args) call-env) (let b (##eval (cadr args) call-env) (if a b ())))))",
    "(##def bind-rec (##macro
        (let self (##eval (car args) call-env)
        (let symbols (##eval (cadr args) call-env)
        (let terms (##eval (caddr args) call-env)
        (let base-env (##eval (cadddr args) call-env)
            (if (symbol? symbols)
                (cons
                    (cons symbols terms)
                    base-env)
                (if (strict-and-pair (pair? symbols) (pair? terms))
                    (cons
                        (cons (car symbols) (car terms))
                        (self self (cdr symbols) (cdr terms) base-env))
                    (if (strict-and-pair (nil? symbols) (nil? terms))
                        base-env
                        err-arg-count-mismatch)))))))))",
    "(##def bind (##macro
        (let symbols (##eval (car args) call-env)
        (let terms (##eval (cadr args) call-env)
        (let base-env (##eval (caddr args) call-env)
            (bind-rec bind-rec symbols terms base-env))))))",
    "(##def lambda (##macro
        (let arg-list (car args)
        (let lambda-body (cadr args)
            (list
                '##closure
                (list
                    'let
                    'arg-values
                    '(eval-all args call-env)
                    (list
                        'let
                        'lambda-env
                        (list
                            'bind
                            (list 'quote arg-list) 
                            'arg-values
                            'captured-env)
                        (list
                            '##eval
                            (list 'quote lambda-body)
                            'lambda-env)))
                call-env)))))",
    "(##def foldl-rec (lambda (self f init list)
        (if (nil? list)
            init
            (self self f (f init (car list)) (cdr list)))))",
    "(##def foldl (lambda (f init list)
        (foldl-rec foldl-rec f init list)))",
    "(##def not (lambda (x) (if x () 't)))",
    "(##def strict-and (lambda args (foldl (lambda (a b) (if a b a)) 't args)))",
    "(##def strict-or (lambda args (foldl (lambda (a b) (if a a b)) () args)))",
    "(##def + (lambda args (foldl (lambda (a b) (##+ a b)) 0 args)))",
    "(##def - (lambda (first . rest) (foldl (lambda (a b) (##- a b)) first rest)))",
    "(##def * (lambda args (foldl (lambda (a b) (##* a b)) 1 args)))",
    "(##def / (lambda (first . rest) (foldl (lambda (a b) (##/ a b)) first rest)))",
    "(##def < (lambda (a b) (##< a b)))",
    "(##def <= (lambda (a b) (strict-or (< a b) (##eq-number? a b))))",
    "(##def > (lambda (a b) (not (<= a b))))",
    "(##def >= (lambda (a b) (not (< a b))))",
    "(##def number? (lambda (x) (##number? x)))",
    "(##def length (lambda (list) (foldl (lambda (a b) (+ a 1)) 0 list)))",
    "(##def apply (##macro
        (let f (##eval (car args) call-env)
        (let args (##eval (cadr args) call-env)
            (##eval (cons (list 'quote f) args) call-env)))))",
    "(##def cond-rec (##macro
        (let self (##eval (car args) call-env)
        (let expr (##eval (cadr args) call-env)
        (let env (##eval (caddr args) call-env)
            (if (nil? expr)
                ()
                (if (##eval (car (car expr)) env)
                    (##eval (cadr (car expr)) env)
                    (self self (cdr expr) env))))))))",
    "(##def cond (##macro (cond-rec cond-rec args call-env)))",
    "(##def eq?-rec (lambda (self a b)
        (cond
            ((strict-and (number? a) (number? b)) (##eq-number? a b))
            ((strict-and (symbol? a) (symbol? a)) (##eq-symbol? a b))
            ((strict-and (nil? a) (nil? b)) 't)
            ((strict-and (pair? a) (pair? b)) (strict-and (self self (car a) (car b)) (self self (cdr a) (cdr b)))))))",
    "(##def eq? (lambda (a b) (eq?-rec eq?-rec a b)))",
    "(##def range-rec (lambda (self a b) (if (< a b) (cons a (self self (+ a 1) b)) ())))",
    "(##def range (lambda (a b) (range-rec range-rec a b)))",
    "(##def reverse (lambda (list) (foldl (lambda (acc x) (cons x acc)) () list)))",
    "(##def map (lambda (f list) (reverse (foldl (lambda (acc x) (cons (f x) acc)) () list))))",
    "(##def eval-here (##macro (##eval (##eval (car args) call-env) call-env)))",
];

fn main() {
    let mut interner = interner::Interner::new();
    let mut eval = eval::Eval::new(&mut interner);
    let mut input = String::new();

    for &builtin in BUILTINS {
        let value = match parser::parse(builtin, &mut interner) {
            Ok(value) => value,
            Err(e) => {
                println!("Failed to parse builtin:\n{:?}", e);
                return;
            }
        };
        if let Err(e) = eval.eval_global(Rc::new(value)) {
            println!("Failed to eval builtin:\n{}", e);
            return;
        }
    }

    loop {
        input.clear();
        print!("> ");
        std::io::stdout().flush().expect("failed to flush");
        std::io::stdin().read_line(&mut input).expect("failed to read");
        let value = match parser::parse(&input, &mut interner) {
            Ok(value) => value,
            Err(e) => {
                println!("Parse error:\n{:?}", e);
                continue;
            }
        };
        match eval.eval_global(Rc::new(value)) {
            Ok(result) => {
                let s = result.to_string();
                let l = if s.len() < 500 { s.len() } else { 500 };
                println!("Result: {}", &s[..l]);
            }
            Err(e) => {
                println!("Eval error:\n{}", e);
            }
        }
    }
}
