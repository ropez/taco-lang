use std::{ops::Range, sync::Arc};

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
    DoubleColon,
    Dot,
    Spread,
    Plus,
    Minus,
    Multiply,
    Divide,
    Identifier(Arc<str>),
    String(Arc<str>),
    Number(i64),
    NewLine,

    // Keywords
    Fun,
    Return,
    Arguments,
    True,
    False,
    If,
    Else,
    For,
    In,
    Rec,
    Enum,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) loc: Range<usize>,
}

struct Tokenizer<'a> {
    src: &'a str,
    loc: Range<usize>,
}

impl<'a> Tokenizer<'a> {
    fn new(src: &'a str) -> Self {
        Self { src, loc: 0..0 }
    }

    fn next_token(&mut self) -> Result<Option<Token>> {
        self.skip_blanks();

        let token = match self.read_char() {
            None => None,
            Some(c) => match c {
                '#' => {
                    // Consume until newline
                    let p = self.remaining().find('\n').expect("end of line");
                    self.skip(p);

                    // FIXME Commenting out thousands of lines results in stack overflow
                    self.next_token()?
                }
                '=' => {
                    if self.take_if_eq('=') {
                        Some(self.produce(TokenKind::Equal))
                    } else {
                        Some(self.produce(TokenKind::Assign))
                    }
                }
                '.' => {
                    if self.take_if_eq('.') {
                        Some(self.produce(TokenKind::Spread))
                    } else {
                        Some(self.produce(TokenKind::Dot))
                    }
                }
                '+' => Some(self.produce(TokenKind::Plus)),
                '-' => Some(self.produce(TokenKind::Minus)),
                '*' => Some(self.produce(TokenKind::Multiply)),
                '/' => Some(self.produce(TokenKind::Divide)),
                '!' => {
                    if self.take_if_eq('=') {
                        Some(self.produce(TokenKind::NotEqual))
                    } else {
                        Some(self.produce(TokenKind::Not))
                    }
                }
                '(' => Some(self.produce(TokenKind::LeftParen)),
                ')' => Some(self.produce(TokenKind::RightParen)),
                '{' => Some(self.produce(TokenKind::LeftBrace)),
                '}' => Some(self.produce(TokenKind::RightBrace)),
                '[' => Some(self.produce(TokenKind::LeftSquare)),
                ']' => Some(self.produce(TokenKind::RightSquare)),
                ',' => Some(self.produce(TokenKind::Comma)),
                ':' => {
                    if self.take_if_eq(':') {
                        Some(self.produce(TokenKind::DoubleColon))
                    } else {
                        Some(self.produce(TokenKind::Colon))
                    }
                }
                '\n' => Some(self.produce(TokenKind::NewLine)),
                '"' => {
                    let s = self.find_str()?;
                    Some(self.produce(TokenKind::String(s)))
                }
                '0'..='9' => {
                    self.untake();
                    let s = self.find_number()?;
                    Some(self.produce(TokenKind::Number(s)))
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    self.untake();
                    let s = self.find_ident();
                    match s.as_ref() {
                        "fun" => Some(self.produce(TokenKind::Fun)),
                        "return" => Some(self.produce(TokenKind::Return)),
                        "arguments" => Some(self.produce(TokenKind::Arguments)),
                        "true" => Some(self.produce(TokenKind::True)),
                        "false" => Some(self.produce(TokenKind::False)),
                        "if" => Some(self.produce(TokenKind::If)),
                        "else" => Some(self.produce(TokenKind::Else)),
                        "for" => Some(self.produce(TokenKind::For)),
                        "in" => Some(self.produce(TokenKind::In)),
                        "rec" => Some(self.produce(TokenKind::Rec)),
                        "enum" => Some(self.produce(TokenKind::Enum)),
                        _ => Some(self.produce(TokenKind::Identifier(s))),
                    }
                }
                _ => {
                    return Err(self.fail("Unexpected token"));
                }
            },
        };

        Ok(token)
    }

    fn skip_blanks(&mut self) {
        let p = self
            .remaining()
            .find(|c| c != ' ')
            .unwrap_or_else(|| self.remaining().len());
        self.skip(p);
    }

    fn remaining(&self) -> &'a str {
        &self.src[self.loc.end..]
    }

    fn peek(&mut self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn read_char(&mut self) -> Option<char> {
        match self.peek() {
            Some(ch) => {
                let l = ch.len_utf8();
                self.loc = self.loc.start..self.loc.end + l;
                Some(ch)
            }
            None => None,
        }
    }

    fn untake(&mut self) {
        self.loc = self.loc.start..self.loc.start;
    }

    fn take_if_eq(&mut self, ch: char) -> bool {
        match self.peek() {
            Some(c) => {
                if c == ch {
                    let l = ch.len_utf8();
                    self.loc = self.loc.start..self.loc.end + l;
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn skip(&mut self, amount: usize) {
        let end = self.loc.end + amount;
        self.loc = end..end;
    }

    fn produce(&mut self, kind: TokenKind) -> Token {
        let loc = self.loc.clone();
        self.loc = self.loc.end..self.loc.end;
        Token {
            kind,
            loc,
        }
    }

    fn fail(&self, msg: &str) -> Error {
        Error::new(msg.into(), self.src, &self.loc)
    }

    fn find_str(&mut self) -> Result<Arc<str>> {
        // FIXME Inefficient
        // FIXME Interpolated strings can contain nested strings
        let mut s = String::new();
        loop {
            match self.read_char() {
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
        self.take_until(|ch| !is_ident_char(ch))
    }

    fn find_number(&mut self) -> Result<i64> {
        let s = self.take_until(|ch| !ch.is_numeric());

        s.parse().map_err(|_| self.fail("Invalid number"))
    }

    fn take_until<P>(&mut self, pattern: P) -> Arc<str>
    where
        P: FnMut(char) -> bool,
    {
        let cur = &self.src[self.loc.start..];
        if let Some(p) = cur.find(pattern) {
            self.loc.end = self.loc.start + p;
            cur[..p].into()
        } else {
            self.loc.end = self.src.len();
            cur[..].into()
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
}

pub fn tokenize(src: &str) -> Result<Vec<Token>> {
    let mut r = Vec::new();
    for token in Tokenizer::new(src) {
        r.push(token?);
    }

    Ok(r)
}

pub(crate) fn is_ident_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}
