use std::{iter::Peekable, str::Chars, sync::Arc};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Assign,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Identifier(Arc<str>),
    String(Box<str>),
    Fun,
    NewLine,
}

struct Tokenizer<'a> {
    code: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    fn new(code: &'a str) -> Self {
        Self {
            code: code.chars().peekable(),
        }
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            match self.code.peek() {
                None => break,
                Some(c) => match c {
                    ' ' => {
                        self.code.next();
                    }
                    '=' => {
                        tokens.push(Token::Assign);
                        self.code.next();
                    }
                    '(' => {
                        tokens.push(Token::LeftParen);
                        self.code.next();
                    }
                    ')' => {
                        tokens.push(Token::RightParen);
                        self.code.next();
                    }
                    '{' => {
                        tokens.push(Token::LeftBrace);
                        self.code.next();
                    }
                    '}' => {
                        tokens.push(Token::RightBrace);
                        self.code.next();
                    }
                    ',' => {
                        tokens.push(Token::Comma);
                        self.code.next();
                    }
                    '\n' => {
                        tokens.push(Token::NewLine);
                        self.code.next();
                    }
                    '"' => {
                        let s = self.get_str();
                        tokens.push(Token::String(s));
                    }
                    'a'..='z' => {
                        let s = self.get_ident();
                        match s.as_ref() {
                            "fun" => tokens.push(Token::Fun),
                            _ => tokens.push(Token::Identifier(s)),
                        };
                    }
                    _ => panic!("Unexpected token: {}", c),
                },
            }
        }

        tokens
    }

    fn get_str(&mut self) -> Box<str> {
        // FIXME: Replace "get" with better verb

        self.code.next(); // TODO Assert "

        // FIXME Inefficient
        let mut s = String::new();
        loop {
            match self.code.next() {
                None => panic!("Unexpected end of input"),
                Some('"') => break,
                Some(c) => s.push(c),
            }
        }

        s.into()
    }

    fn get_ident(&mut self) -> Arc<str> {
        // FIXME: Replace "get" with better verb

        // FIXME Inefficient
        let mut s = String::new();
        loop {
            match self.code.peek() {
                None => panic!("Unexpected end of input"),
                Some(c) => match c {
                    'a'..='z' => {
                        s.push(*c);
                        self.code.next();
                    }
                    _ => break,
                }
            }
        }

        s.into()
    }
}

pub fn tokenize(code: &str) -> Vec<Token> {
    Tokenizer::new(code).tokenize()
}
