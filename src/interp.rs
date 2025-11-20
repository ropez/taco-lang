#![allow(unused)]

use std::ops::Range;

use crate::lexer::is_ident_char;

#[derive(Debug, PartialEq)]
pub enum StringTokenKind {
    Str,
    Expr,
}

#[derive(Debug, PartialEq)]
pub struct StringToken<'a> {
    pub(crate) kind: StringTokenKind,
    pub(crate) src: &'a str,
    pub(crate) offset: usize,
}

impl<'a> StringToken<'a> {
    fn string(src: &'a str, offset: usize) -> Self {
        Self {
            kind: StringTokenKind::Str,
            src,
            offset,
        }
    }

    fn expr(src: &'a str, offset: usize) -> Self {
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
                Some(_) => panic!("Unespected character after $"),
            }
        } else {
            tokens.push(StringToken::string(cur, pos));
            break;
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_returns_empty_string() {
        let res = tokenise_string("");

        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_returns_whole_string() {
        let res = tokenise_string("foobar");

        assert_eq!(res, vec![StringToken::string("foobar", 0)]);
    }

    #[test]
    fn test_returns_string_with_dollar_signs() {
        let res = tokenise_string("$$foo$$bar$$");

        assert_eq!(
            res,
            vec![
                StringToken::string("$foo", 0),
                StringToken::string("$bar", 5),
                StringToken::string("$", 10),
            ]
        );
    }

    #[test]
    fn test_returns_single_expression() {
        let res = tokenise_string("$foobar");

        assert_eq!(res, vec![StringToken::expr("foobar", 1)]);
    }

    #[test]
    fn test_returns_multiple_expressions() {
        let res = tokenise_string("$foo$bar");

        assert_eq!(
            res,
            vec![StringToken::expr("foo", 1), StringToken::expr("bar", 5)]
        );
    }

    #[test]
    fn test_returns_string_and_then_expression() {
        let res = tokenise_string("√ Item: $foo");

        assert_eq!(
            res,
            vec![
                StringToken::string("√ Item: ", 0),
                StringToken::expr("foo", 11)
            ]
        );
    }

    #[test]
    fn test_returns_expression_and_then_string() {
        let res = tokenise_string("$foo is the item √");

        assert_eq!(
            res,
            vec![
                StringToken::expr("foo", 1),
                StringToken::string(" is the item √", 4)
            ]
        );
    }

    #[test]
    fn test_returns_expression_followed_by_punctuation() {
        let res = tokenise_string("$foo, said the troll");

        assert_eq!(
            res,
            vec![
                StringToken::expr("foo", 1),
                StringToken::string(", said the troll", 4),
            ]
        );
    }

    #[test]
    fn test_returns_expression_inside_braces() {
        let res = tokenise_string("foo${koko + foo.item()}bar");

        assert_eq!(
            res,
            vec![
                StringToken::string("foo", 0),
                StringToken::expr("koko + foo.item()", 5),
                StringToken::string("bar", 23),
            ]
        );
    }

    #[test]
    fn test_returns_expression_inside_braces_with_non_ascii() {
        let res = tokenise_string("√ Item: ${foo.item()}--√");

        assert_eq!(
            res,
            vec![
                StringToken::string("√ Item: ", 0),
                StringToken::expr("foo.item()", 12),
                StringToken::string("--√", 23),
            ]
        );
    }
}
