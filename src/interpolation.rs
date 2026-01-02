#![allow(unused)]

use std::ops::Range;

use crate::lexer::is_ident_char;

#[derive(Debug, PartialEq)]
pub enum StringTokenKind {
    Str,
    Expr,
}

#[derive(Debug, PartialEq)]
pub(crate) struct StringToken<'a> {
    pub(crate) kind: StringTokenKind,
    pub(crate) src: &'a str,
    pub(crate) offset: usize,
}

impl<'a> StringToken<'a> {
    pub(crate) fn string(src: &'a str, offset: usize) -> Self {
        Self {
            kind: StringTokenKind::Str,
            src,
            offset,
        }
    }

    pub(crate) fn expr(src: &'a str, offset: usize) -> Self {
        Self {
            kind: StringTokenKind::Expr,
            src,
            offset,
        }
    }
}

pub fn tokenise_string<'a>(src: &'a str) -> Vec<StringToken<'a>> {
    let mut tokens = Vec::new();

    let mut pos = 0;
    while pos < src.len() {
        let cur = &src[pos..];
        if let Some(p) = cur.find('$') {
            if p > 0 {
                tokens.push(StringToken::string(&cur[..p], pos));
            }

            pos += p + 1;
            let cur = &cur[p + 1..];

            match cur.chars().next() {
                None => panic!("Unexpected end of string"),
                Some('A'..='Z' | 'a'..='z') => {
                    if let Some(p) = cur.find(|ch: char| !is_ident_char(ch)) {
                        let (l, h) = cur.split_at(p);
                        tokens.push(StringToken::expr(l, pos));

                        pos += p;
                    } else {
                        tokens.push(StringToken::expr(cur, pos));
                        break;
                    }
                }
                Some('{') => {
                    // XXX Support nested blocks
                    if let Some(p) = cur.find('}') {
                        let (l, h) = cur.split_at(p + 1);
                        tokens.push(StringToken::expr(l.trim_matches(['{', '}']), pos + 1));

                        pos += p + 1;
                    } else {
                        panic!("Unterminated block in string")
                    }
                }
                Some('$') => {
                    if let Some(p) = cur[1..].find('$') {
                        tokens.push(StringToken::string(&cur[..p + 1], pos - 1));
                        pos += p + 1
                    } else {
                        tokens.push(StringToken::string(cur, pos - 1));
                        break;
                    }
                }
                Some(_) => panic!("Unexpected character after $"),
            }
        } else {
            tokens.push(StringToken::string(cur, pos));
            break;
        }
    }

    tokens
}
