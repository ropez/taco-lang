use std::sync::Arc;

use crate::error::{Error, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) kind: TokenKind,
    // pub(crate) src: &'a str,
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
                            tokens.push(self.produce(TokenKind::Equal));
                        } else {
                            tokens.push(self.produce(TokenKind::Assign));
                        }
                    }
                    '.' => {
                        if self.take_if_eq('.') {
                            tokens.push(self.produce(TokenKind::Spread));
                        } else {
                            tokens.push(self.produce(TokenKind::Dot));
                        }
                    }
                    '!' => {
                        if self.take_if_eq('=') {
                            tokens.push(self.produce(TokenKind::NotEqual));
                        } else {
                            tokens.push(self.produce(TokenKind::Not));
                        }
                    }
                    '(' => {
                        tokens.push(self.produce(TokenKind::LeftParen));
                    }
                    ')' => {
                        tokens.push(self.produce(TokenKind::RightParen));
                    }
                    '{' => {
                        tokens.push(self.produce(TokenKind::LeftBrace));
                    }
                    '}' => {
                        tokens.push(self.produce(TokenKind::RightBrace));
                    }
                    '[' => {
                        tokens.push(self.produce(TokenKind::LeftSquare));
                    }
                    ']' => {
                        tokens.push(self.produce(TokenKind::RightSquare));
                    }
                    ',' => {
                        tokens.push(self.produce(TokenKind::Comma));
                    }
                    ':' => {
                        tokens.push(self.produce(TokenKind::Colon));
                    }
                    '\n' => {
                        tokens.push(self.produce(TokenKind::NewLine));
                    }
                    '"' => {
                        let s = self.find_str()?;
                        tokens.push(self.produce(TokenKind::String(s)));
                    }
                    '0'..='9' => {
                        let s = self.find_number()?;
                        tokens.push(self.produce(TokenKind::Number(s)));
                    }
                    'A'..='Z' | 'a'..='z' => {
                        let s = self.find_ident();
                        match s.as_ref() {
                            "fun" => tokens.push(self.produce(TokenKind::Fun)),
                            "return" => tokens.push(self.produce(TokenKind::Return)),
                            "if" => tokens.push(self.produce(TokenKind::If)),
                            "else" => tokens.push(self.produce(TokenKind::Else)),
                            "for" => tokens.push(self.produce(TokenKind::For)),
                            "in" => tokens.push(self.produce(TokenKind::In)),
                            "record" => tokens.push(self.produce(TokenKind::Record)),
                            _ => tokens.push(self.produce(TokenKind::Identifier(s))),
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

    fn produce(&mut self, kind: TokenKind) -> Token {
        // let s = extract_str(self.lcur, self.cur);
        self.lcur = self.cur;
        Token { kind }
    }

    fn fail(&self, msg: &str) -> Error {
        Error::new(msg.into(), self.src, extract_str(self.cur, self.lcur))
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

fn extract_str<'a>(src: &'a str, cur: &'a str) -> &'a str {
    let start = src.as_ptr() as usize;
    let end = cur.as_ptr() as usize;
    assert!(end >= start);
    let off = end - start;
    &src[..off]
}
