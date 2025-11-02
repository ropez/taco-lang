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
    Comma,
    Identifier(Arc<str>),
    String(Arc<str>),
    Fun,
    For,
    In,
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
                    '#' => {
                        // Consume until newline
                        while self.code.next_if(|t| *t != '\n').is_some() {}
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
                    '\n' => {
                        tokens.push(Token::NewLine);
                        self.code.next();
                    }
                    '"' => {
                        let s = self.find_str();
                        tokens.push(Token::String(s));
                    }
                    'a'..='z' => {
                        let s = self.find_ident();
                        match s.as_ref() {
                            "fun" => tokens.push(Token::Fun),
                            "for" => tokens.push(Token::For),
                            "in" => tokens.push(Token::In),
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
