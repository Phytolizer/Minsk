use crate::analysis::diagnostic_bag::DiagnosticBag;
use crate::analysis::text_span::TextSpan;
use crate::object::Object;

use super::facts;
use super::kind::SyntaxKind;
use super::node::SyntaxNode;
use super::token::SyntaxToken;

pub(crate) struct Lexer {
    text: Vec<char>,
    diagnostics: DiagnosticBag,
}

impl Lexer {
    pub(crate) fn new(text: &str) -> Self {
        Self {
            text: text.chars().collect(),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub(crate) fn into_tokens_and_diagnostics(mut self) -> (Vec<SyntaxToken>, DiagnosticBag) {
        let tokens = LexerIterator::new(&mut self)
            .filter(|tok| {
                ![SyntaxKind::WhitespaceToken, SyntaxKind::BadToken].contains(&tok.kind())
            })
            .collect::<Vec<_>>();
        (tokens, self.diagnostics)
    }
}

struct LexerIterator<'a> {
    lex: &'a mut Lexer,
    position: usize,
}

impl<'a> LexerIterator<'a> {
    fn new(lex: &'a mut Lexer) -> Self {
        Self { lex, position: 0 }
    }

    fn peek(&self, offset: usize) -> char {
        self.lex
            .text
            .get(self.position + offset)
            .copied()
            .unwrap_or('\0')
    }

    fn current(&self) -> char {
        self.peek(0)
    }

    fn current_text(&self, start: usize) -> String {
        self.lex.text[start..self.position].iter().collect()
    }
}

impl<'a> Iterator for LexerIterator<'a> {
    type Item = SyntaxToken;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.position;
        let mut current_text: Option<String> = None;
        let mut kind = SyntaxKind::BadToken;
        let mut value: Option<Object> = None;

        match self.current() {
            '\0' => return None,
            c if c.is_whitespace() => {
                while self.current().is_whitespace() {
                    self.position += 1;
                }
                kind = SyntaxKind::WhitespaceToken;
            }
            c if c.is_digit(10) => {
                while self.current().is_digit(10) {
                    self.position += 1;
                }
                kind = SyntaxKind::NumberToken;
                current_text = Some(self.current_text(start));
                let int_val = current_text.as_ref().unwrap().parse::<i64>();
                match int_val {
                    Ok(iv) => {
                        value = Some(Object::Number(iv));
                    }
                    Err(_) => {
                        self.lex.diagnostics.report_invalid_i64(
                            TextSpan::new(start, self.position - start),
                            current_text.as_ref().unwrap(),
                        );
                    }
                }
            }
            c if c.is_alphabetic() => {
                while self.current().is_alphanumeric() {
                    self.position += 1;
                }
                current_text = Some(self.current_text(start));
                kind = facts::keyword_kind(current_text.as_ref().unwrap());
            }
            '+' => {
                kind = SyntaxKind::PlusToken;
                self.position += 1;
            }
            '-' => {
                kind = SyntaxKind::MinusToken;
                self.position += 1;
            }
            '*' => {
                kind = SyntaxKind::StarToken;
                self.position += 1;
            }
            '/' => {
                kind = SyntaxKind::SlashToken;
                self.position += 1;
            }
            '(' => {
                kind = SyntaxKind::OpenParenthesisToken;
                self.position += 1;
            }
            ')' => {
                kind = SyntaxKind::CloseParenthesisToken;
                self.position += 1;
            }
            '!' => {
                if self.peek(1) == '=' {
                    kind = SyntaxKind::BangEqualsToken;
                    self.position += 2;
                } else {
                    kind = SyntaxKind::BangToken;
                    self.position += 1;
                }
            }
            '=' => {
                if self.peek(1) == '=' {
                    kind = SyntaxKind::EqualsEqualsToken;
                    self.position += 2;
                }
            }
            '&' => {
                if self.peek(1) == '&' {
                    kind = SyntaxKind::AmpersandAmpersandToken;
                    self.position += 2;
                }
            }
            '|' => {
                if self.peek(1) == '|' {
                    kind = SyntaxKind::PipePipeToken;
                    self.position += 2;
                }
            }
            _ => {}
        }

        if kind == SyntaxKind::BadToken {
            self.lex
                .diagnostics
                .report_bad_character(self.position, self.current());
            self.position += 1;
        }

        Some(SyntaxToken::new(
            kind,
            current_text.unwrap_or_else(|| self.current_text(start)),
            start,
            self.position - start,
            value,
        ))
    }
}
