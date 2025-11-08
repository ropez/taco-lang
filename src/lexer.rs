use std::sync::Arc;

use crate::error::{Error, Result};

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
    src: &'a str,
    lcur: &'a str,
    cur: &'a str,
}

impl<'a> Tokenizer<'a> {
    fn new(src: &'a str) -> Self {
        Self { src, lcur: src, cur: src }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            match self.take() {
                None => break,
                Some(c) => match c {
                    ' ' => {
                        self.skip();
                    }
                    '#' => {
                        // Consume until newline
                        // XXX Combine these lines:
                        let p = self.cur.find('\n').expect("end of line");
                        let (_, c) = self.cur.split_at(p);
                        self.cur = c;
                        self.skip();
                    }
                    '=' => {
                        if self.take_if_eq('=') {
                            tokens.push(self.produce(Token::Equal));
                        } else {
                            tokens.push(self.produce(Token::Assign));
                        }
                    }
                    '.' => {
                        if self.take_if_eq('.') {
                            tokens.push(self.produce(Token::Spread));
                        } else {
                            tokens.push(self.produce(Token::Dot));
                        }
                    }
                    '!' => {
                        if self.take_if_eq('=') {
                            tokens.push(self.produce(Token::NotEqual));
                        } else {
                            tokens.push(self.produce(Token::Not));
                        }
                    }
                    '(' => {
                        tokens.push(self.produce(Token::LeftParen));
                    }
                    ')' => {
                        tokens.push(self.produce(Token::RightParen));
                    }
                    '{' => {
                        tokens.push(self.produce(Token::LeftBrace));
                    }
                    '}' => {
                        tokens.push(self.produce(Token::RightBrace));
                    }
                    '[' => {
                        tokens.push(self.produce(Token::LeftSquare));
                    }
                    ']' => {
                        tokens.push(self.produce(Token::RightSquare));
                    }
                    ',' => {
                        tokens.push(self.produce(Token::Comma));
                    }
                    ':' => {
                        tokens.push(self.produce(Token::Colon));
                    }
                    '\n' => {
                        tokens.push(self.produce(Token::NewLine));
                    }
                    '"' => {
                        let s = self.find_str()?;
                        tokens.push(self.produce(Token::String(s)));
                    }
                    '0'..='9' => {
                        let s = self.find_number()?;
                        tokens.push(self.produce(Token::Number(s)));
                    }
                    'A'..='Z' | 'a'..='z' => {
                        let s = self.find_ident();
                        match s.as_ref() {
                            "fun" => tokens.push(self.produce(Token::Fun)),
                            "return" => tokens.push(self.produce(Token::Return)),
                            "if" => tokens.push(self.produce(Token::If)),
                            "else" => tokens.push(self.produce(Token::Else)),
                            "for" => tokens.push(self.produce(Token::For)),
                            "in" => tokens.push(self.produce(Token::In)),
                            "record" => tokens.push(self.produce(Token::Record)),
                            _ => tokens.push(self.produce(Token::Identifier(s))),
                        };
                    }
                    _ => {
                        return Err(self.fail("Unexpected token"));
                    }
                },
            }
        }

        Ok(tokens)
    }

    fn peek(&mut self) -> Option<char> {
        self.cur.chars().next()
    }

    fn take(&mut self) -> Option<char> {
        match self.peek() {
            Some(ch) => {
                let l = ch.len_utf8();
                self.cur = &self.cur[l..];
                Some(ch)
            }
            None => None,
        }
    }

    fn take_if_eq(&mut self, ch: char) -> bool {
        match self.peek() {
            Some(c) => {
                if c == ch {
                    let l = ch.len_utf8();
                    self.cur = &self.cur[l..];
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn skip(&mut self) {
        self.lcur = self.cur;
    }

    fn produce(&mut self, token: Token) -> Token {
        self.lcur = self.cur;
        token
    }

    fn fail(&self, msg: &str) -> Error {
        let off = (self.cur.as_ptr() as usize) - (self.lcur.as_ptr() as usize);
        Error::new(msg.into(), self.src, &self.lcur[..off])
    }

    fn find_str(&mut self) -> Result<Arc<str>> {
        // self.take(); // TODO Assert "

        // FIXME Inefficient
        let mut s = String::new();
        loop {
            match self.take() {
                None => {
                    return Err(self.fail("Unexpected end of input"));
                }
                Some('"') => break,
                Some(c) => s.push(c),
            }
        }

        Ok(s.into())
    }

    fn find_ident(&mut self) -> Arc<str> {
        // FIXME Inefficient
        self.cur = self.lcur;
        let mut s = String::new();
        loop {
            match self.peek() {
                None => break,
                Some(c) => match c {
                    'A'..='Z' | 'a'..='z' => {
                        s.push(c);
                        self.take();
                    }
                    _ => break,
                },
            }
        }

        s.into()
    }

    fn find_number(&mut self) -> Result<i64> {
        // FIXME Inefficient
        self.cur = self.lcur;
        let mut s = String::new();
        loop {
            match self.peek() {
                None => break,
                Some(c) => match c {
                    '0'..='9' => {
                        s.push(c);
                        self.take();
                    }
                    _ => break,
                },
            }
        }

        s.parse().map_err(|_| self.fail("Invalid number"))
    }
}

pub fn tokenize(src: &str) -> Result<Vec<Token>> {
    Tokenizer::new(src).tokenize()
}
