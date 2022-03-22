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

    pub(crate) fn into_only_tokens(mut self) -> Vec<SyntaxToken> {
        LexerIterator::new(&mut self).collect::<Vec<_>>()
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
                } else {
                    kind = SyntaxKind::EqualsToken;
                    self.position += 1;
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

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use crate::analysis::syntax::facts;
    use crate::analysis::syntax::kind::SyntaxKind;
    use crate::analysis::syntax::tree::SyntaxTree;

    #[derive(Clone, Copy)]
    struct SimpleToken {
        kind: SyntaxKind,
        text: &'static str,
    }

    impl SimpleToken {
        const fn new(kind: SyntaxKind, text: &'static str) -> Self {
            Self { kind, text }
        }
    }

    fn get_tokens() -> impl Iterator<Item = SimpleToken> {
        SyntaxKind::iter()
            .filter_map(|kind| {
                if let Some(text) = facts::get_text(kind) {
                    Some(SimpleToken::new(kind, text))
                } else {
                    None
                }
            })
            .chain(
                [
                    SimpleToken::new(SyntaxKind::IdentifierToken, "a"),
                    SimpleToken::new(SyntaxKind::IdentifierToken, "abc"),
                    SimpleToken::new(SyntaxKind::NumberToken, "1"),
                    SimpleToken::new(SyntaxKind::NumberToken, "123"),
                ]
                .into_iter(),
            )
    }

    const SEPARATORS: &[SimpleToken] = &[
        SimpleToken::new(SyntaxKind::WhitespaceToken, " "),
        SimpleToken::new(SyntaxKind::WhitespaceToken, "  "),
        SimpleToken::new(SyntaxKind::WhitespaceToken, "\n"),
        SimpleToken::new(SyntaxKind::WhitespaceToken, "\r"),
        SimpleToken::new(SyntaxKind::WhitespaceToken, "\r\n"),
    ];

    #[test]
    fn lexes_token() {
        for tt in get_tokens() {
            let SimpleToken { kind, text } = tt;
            let tokens = SyntaxTree::parse_tokens(text);
            assert_eq!(1, tokens.len());
            let token = &tokens[0];
            assert_eq!(kind, token.kind);
            assert_eq!(text, token.text);
        }
    }

    #[test]
    fn lexes_token_pairs() {
        for t1 in get_tokens() {
            for t2 in get_tokens() {
                if !requires_separator(&t1, &t2) {
                    let text = format!("{}{}", t1.text, t2.text);
                    let tokens = SyntaxTree::parse_tokens(&text);
                    assert_eq!(2, tokens.len());
                    assert_eq!(t1.kind, tokens[0].kind);
                    assert_eq!(t1.text, tokens[0].text);
                    assert_eq!(t2.kind, tokens[1].kind);
                    assert_eq!(t2.text, tokens[1].text);
                }
            }
        }
    }

    fn requires_separator(t1: &SimpleToken, t2: &SimpleToken) -> bool {
        let t1_is_keyword = format!("{:?}", t1.kind).ends_with("Keyword");
        let t2_is_keyword = format!("{:?}", t2.kind).ends_with("Keyword");

        if (t1.kind == SyntaxKind::IdentifierToken || t1_is_keyword)
            && (t2.kind == SyntaxKind::IdentifierToken || t2_is_keyword)
        {
            true
        } else if (t1.kind == SyntaxKind::NumberToken
            || t1.kind == SyntaxKind::IdentifierToken
            || t1_is_keyword)
            && t2.kind == SyntaxKind::NumberToken
        {
            true
        } else if (t1.kind == SyntaxKind::BangToken || t1.kind == SyntaxKind::EqualsToken)
            && (t2.kind == SyntaxKind::EqualsToken || t2.kind == SyntaxKind::EqualsEqualsToken)
        {
            true
        } else {
            false
        }
    }

    #[test]
    fn lexes_token_pairs_with_separator() {
        for t1 in get_tokens() {
            for t2 in get_tokens() {
                if requires_separator(&t1, &t2) {
                    for sep in SEPARATORS {
                        let text = format!("{}{}{}", t1.text, sep.text, t2.text);
                        let tokens = SyntaxTree::parse_tokens(&text);
                        assert_eq!(3, tokens.len());
                        assert_eq!(t1.kind, tokens[0].kind);
                        assert_eq!(t1.text, tokens[0].text);
                        assert_eq!(sep.kind, tokens[1].kind);
                        assert_eq!(sep.text, tokens[1].text);
                        assert_eq!(t2.kind, tokens[2].kind);
                        assert_eq!(t2.text, tokens[2].text);
                    }
                }
            }
        }
    }
}
