use std::{iter::Peekable, str::Chars, sync::Arc};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Assign,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquare,
    RightSquare,
    Not,
    Equal,
    NotEqual,
    Comma,
    Colon,
    Dot,
    Spread,
    Identifier(Arc<str>),
    String(Arc<str>),
    Number(i64),
    NewLine,

    // Keywords
    Fun,
    Return,
    If,
    Else,
    For,
    In,
    Record,
}

struct Tokenizer<'a> {
    code: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            code: src.chars().peekable(),
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
                    '#' => {
                        // Consume until newline
                        while self.code.next_if(|t| *t != '\n').is_some() {}
                    }
                    '=' => {
                        self.code.next();
                        if self.code.next_if_eq(&'=').is_some() {
                            tokens.push(Token::Equal);
                        } else {
                            tokens.push(Token::Assign);
                        }
                    }
                    '.' => {
                        self.code.next();
                        if self.code.next_if_eq(&'.').is_some() {
                            tokens.push(Token::Spread);
                        } else {
                            tokens.push(Token::Dot);
                        }
                    }
                    '!' => {
                        self.code.next();
                        if self.code.next_if_eq(&'=').is_some() {
                            tokens.push(Token::NotEqual);
                        } else {
                            tokens.push(Token::Not);
                        }
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
                    '[' => {
                        tokens.push(Token::LeftSquare);
                        self.code.next();
                    }
                    ']' => {
                        tokens.push(Token::RightSquare);
                        self.code.next();
                    }
                    ',' => {
                        tokens.push(Token::Comma);
                        self.code.next();
                    }
                    ':' => {
                        tokens.push(Token::Colon);
                        self.code.next();
                    }
                    '\n' => {
                        tokens.push(Token::NewLine);
                        self.code.next();
                    }
                    '"' => {
                        let s = self.find_str();
                        tokens.push(Token::String(s));
                    }
                    '0'..='9' => {
                        let s = self.find_number();
                        tokens.push(Token::Number(s));
                    }
                    'A'..='Z' | 'a'..='z' => {
                        let s = self.find_ident();
                        match s.as_ref() {
                            "fun" => tokens.push(Token::Fun),
                            "return" => tokens.push(Token::Return),
                            "if" => tokens.push(Token::If),
                            "else" => tokens.push(Token::Else),
                            "for" => tokens.push(Token::For),
                            "in" => tokens.push(Token::In),
                            "record" => tokens.push(Token::Record),
                            _ => tokens.push(Token::Identifier(s)),
                        };
                    }
                    _ => panic!("Unexpected token: {}", c),
                },
            }
        }

        tokens
    }

    fn find_str(&mut self) -> Arc<str> {
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

    fn find_ident(&mut self) -> Arc<str> {
        // FIXME Inefficient
        let mut s = String::new();
        loop {
            match self.code.peek() {
                None => break,
                Some(c) => match c {
                    'A'..='Z' | 'a'..='z' => {
                        s.push(*c);
                        self.code.next();
                    }
                    _ => break,
                }
            }
        }

        s.into()
    }

    fn find_number(&mut self) -> i64 {
        // FIXME Inefficient
        let mut s = String::new();
        loop {
            match self.code.peek() {
                None => break,
                Some(c) => match c {
                    '0'..='9' => {
                        s.push(*c);
                        self.code.next();
                    }
                    _ => break,
                }
            }
        }

        s.parse().expect("parse number")
    }
}

pub fn tokenize(src: &str) -> Vec<Token> {
    Tokenizer::new(src).tokenize()
}
