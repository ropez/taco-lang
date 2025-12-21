use std::{fmt, ops, result, sync::Arc};

use crate::{
    error::{ParseError, ParseErrorKind},
    ident::Ident,
};

type Result<T> = result::Result<T, ParseError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl Loc {
    pub const fn start() -> Self {
        Self { start: 0, end: 0 }
    }

    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn shift_right(self, offset: usize) -> Self {
        Self::new(self.start + offset, self.end + offset)
    }
}

#[derive(Clone)]
pub struct Src<T> {
    inner: T,
    pub(crate) loc: Loc,
}

impl<T> Src<T>
where
    T: Clone,
{
    pub fn cloned(&self) -> T {
        self.inner.clone()
    }
}

impl<T> Src<T> {
    pub fn into_inner(self) -> T {
        self.inner
    }
}

impl<T> fmt::Debug for Src<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl<T> Src<T> {
    pub(crate) fn new(expr: T, loc: Loc) -> Self {
        Self { inner: expr, loc }
    }
}

impl<T> AsRef<T> for Src<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> ops::Deref for Src<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Assign,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquare,
    RightSquare,
    Question,
    Coalesce,
    Alpha,
    Exclamation,
    Pipe,
    LogicAnd,
    LogicOr,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,
    Comma,
    Colon,
    DoubleColon,
    Dot,
    Spread,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Identifier(Ident),
    String(Arc<str>),
    Number(i64),
    NewLine,
    Comment(Arc<str>),

    // Keywords
    Fun,
    Return,
    Arguments,
    True,
    False,
    If,
    Else,
    While,
    For,
    In,
    Rec,
    Enum,
    Match,
    Assert,
}

pub type Token = Src<TokenKind>;

struct Tokenizer<'a> {
    src: &'a str,
    loc: Loc,
}

impl<'a> Tokenizer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            loc: Loc::start(),
        }
    }

    fn next_token(&mut self) -> Result<Option<Token>> {
        self.skip_blanks();

        let token = match self.read_char() {
            None => None,
            Some(c) => match c {
                '#' => {
                    let s = self.take_until(|c| c == '\n').into();
                    Some(self.produce(TokenKind::Comment(s)))
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
                '%' => Some(self.produce(TokenKind::Modulo)),
                '@' => Some(self.produce(TokenKind::Alpha)),
                '!' => {
                    if self.take_if_eq('=') {
                        Some(self.produce(TokenKind::NotEqual))
                    } else {
                        Some(self.produce(TokenKind::Exclamation))
                    }
                }
                '&' => {
                    if self.take_if_eq('&') {
                        Some(self.produce(TokenKind::LogicAnd))
                    } else {
                        return Err(ParseError::unexpected_token().at(self.loc));
                    }
                }
                '|' => {
                    if self.take_if_eq('|') {
                        Some(self.produce(TokenKind::LogicOr))
                    } else {
                        Some(self.produce(TokenKind::Pipe))
                    }
                }
                '(' => Some(self.produce(TokenKind::LeftParen)),
                ')' => Some(self.produce(TokenKind::RightParen)),
                '{' => Some(self.produce(TokenKind::LeftBrace)),
                '}' => Some(self.produce(TokenKind::RightBrace)),
                '[' => Some(self.produce(TokenKind::LeftSquare)),
                ']' => Some(self.produce(TokenKind::RightSquare)),
                '?' => {
                    if self.take_if_eq('?') {
                        Some(self.produce(TokenKind::Coalesce))
                    } else {
                        Some(self.produce(TokenKind::Question))
                    }
                }
                '<' => {
                    if self.take_if_eq('=') {
                        Some(self.produce(TokenKind::LessOrEqual))
                    } else {
                        Some(self.produce(TokenKind::LessThan))
                    }
                }
                '>' => {
                    if self.take_if_eq('=') {
                        Some(self.produce(TokenKind::GreaterOrEqual))
                    } else {
                        Some(self.produce(TokenKind::GreaterThan))
                    }
                }
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
                    match s.as_str() {
                        "fun" => Some(self.produce(TokenKind::Fun)),
                        "return" => Some(self.produce(TokenKind::Return)),
                        "arguments" => Some(self.produce(TokenKind::Arguments)),
                        "true" => Some(self.produce(TokenKind::True)),
                        "false" => Some(self.produce(TokenKind::False)),
                        "if" => Some(self.produce(TokenKind::If)),
                        "else" => Some(self.produce(TokenKind::Else)),
                        "while" => Some(self.produce(TokenKind::While)),
                        "for" => Some(self.produce(TokenKind::For)),
                        "in" => Some(self.produce(TokenKind::In)),
                        "rec" => Some(self.produce(TokenKind::Rec)),
                        "enum" => Some(self.produce(TokenKind::Enum)),
                        "match" => Some(self.produce(TokenKind::Match)),
                        "assert" => Some(self.produce(TokenKind::Assert)),
                        _ => Some(self.produce(TokenKind::Identifier(s))),
                    }
                }
                _ => {
                    return Err(ParseError::unexpected_token().at(self.loc));
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
                self.loc = Loc::new(self.loc.start, self.loc.end + l);
                Some(ch)
            }
            None => None,
        }
    }

    fn untake(&mut self) {
        self.loc = Loc::new(self.loc.start, self.loc.start);
    }

    fn take_if_eq(&mut self, ch: char) -> bool {
        match self.peek() {
            Some(c) => {
                if c == ch {
                    let l = ch.len_utf8();
                    self.loc = Loc::new(self.loc.start, self.loc.end + l);
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
        self.loc = Loc::new(end, end);
    }

    fn produce(&mut self, kind: TokenKind) -> Token {
        let loc = self.loc;
        self.loc = Loc::new(self.loc.end, self.loc.end);
        Token { inner: kind, loc }
    }

    fn find_str(&mut self) -> Result<Arc<str>> {
        // FIXME Inefficient
        // FIXME Interpolated strings can contain nested strings
        let mut s = String::new();
        loop {
            match self.read_char() {
                None => {
                    return Err(ParseError::new(ParseErrorKind::UnexpectedEndOfInput).at(self.loc));
                }
                Some('"') => break,
                Some(c) => s.push(c),
            }
        }

        Ok(s.into())
    }

    fn find_ident(&mut self) -> Ident {
        self.take_until(|ch| !is_ident_char(ch)).into()
    }

    fn find_number(&mut self) -> Result<i64> {
        let s = self.take_until(|ch| !ch.is_numeric());

        s.parse()
            .map_err(|_| ParseError::new(ParseErrorKind::InvalidNumber).at(self.loc))
    }

    fn take_until<P>(&mut self, pattern: P) -> &str
    where
        P: FnMut(char) -> bool,
    {
        let cur = &self.src[self.loc.start..];
        if let Some(p) = cur.find(pattern) {
            self.loc.end = self.loc.start + p;
            &cur[..p]
        } else {
            self.loc.end = self.src.len();
            cur
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
