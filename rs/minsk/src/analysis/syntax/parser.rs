use crate::analysis::diagnostic_bag::DiagnosticBag;
use crate::object::Object;

use super::facts;
use super::kind::SyntaxKind;
use super::lexer::Lexer;
use super::node::expression::binary::BinaryExpressionSyntax;
use super::node::expression::literal::LiteralExpressionSyntax;
use super::node::expression::parenthesized::ParenthesizedExpressionSyntax;
use super::node::expression::unary::UnaryExpressionSyntax;
use super::node::expression::ExpressionSyntax;
use super::node::SyntaxNode;
use super::token::SyntaxToken;
use super::tree::SyntaxTree;

pub(crate) struct Parser {
    tokens: Vec<SyntaxToken>,
    diagnostics: DiagnosticBag,
    position: usize,
}

impl Parser {
    pub(crate) fn new(text: &str) -> Self {
        let lex = Lexer::new(text);
        let (tokens, diagnostics) = lex.into_tokens_and_diagnostics();
        Self {
            tokens,
            diagnostics,
            position: 0,
        }
    }

    fn peek(&self, offset: usize) -> SyntaxToken {
        let index = self.position + offset;
        if index < self.tokens.len() {
            return self.tokens[index].clone();
        }

        let last_position = if self.tokens.len() > 0 {
            self.tokens.last().unwrap().span().end()
        } else {
            0
        };

        SyntaxToken::new(
            SyntaxKind::EndOfFileToken,
            String::new(),
            last_position,
            1,
            None,
        )
    }

    fn current(&self) -> SyntaxToken {
        self.peek(0)
    }

    fn peek_kind(&self, offset: usize) -> SyntaxKind {
        let index = self.position + offset;
        if index < self.tokens.len() {
            self.tokens[index].kind()
        } else {
            SyntaxKind::EndOfFileToken
        }
    }

    fn current_kind(&self) -> SyntaxKind {
        self.peek_kind(0)
    }

    fn next_token(&mut self) -> SyntaxToken {
        let current = self.current();
        self.position += 1;
        current
    }

    fn match_token(&mut self, kind: SyntaxKind) -> SyntaxToken {
        let tok = self.current();
        if tok.kind() == kind {
            self.position += 1;
            tok
        } else {
            self.diagnostics
                .report_unexpected_token(tok.span(), kind, tok.kind());
            SyntaxToken::new(kind, String::new(), tok.position(), tok.length(), None)
        }
    }

    pub(crate) fn parse(mut self) -> SyntaxTree {
        SyntaxTree::new(
            self.parse_expression(),
            self.match_token(SyntaxKind::EndOfFileToken),
            self.diagnostics.into_iter().collect(),
        )
    }

    fn parse_expression(&mut self) -> ExpressionSyntax {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, parent_precedence: usize) -> ExpressionSyntax {
        let unary_operator_precedence = facts::unary_operator_precedence(self.current_kind());
        let mut left =
            if unary_operator_precedence != 0 && unary_operator_precedence >= parent_precedence {
                let operator_token = self.next_token();
                let operand = Box::new(self.parse_binary_expression(unary_operator_precedence));
                ExpressionSyntax::Unary(UnaryExpressionSyntax {
                    operator_token,
                    operand,
                })
            } else {
                self.parse_primary_expression()
            };

        loop {
            let precedence = facts::binary_operator_precedence(self.current_kind());
            if precedence == 0 || precedence <= parent_precedence {
                break;
            }

            let operator_token = self.next_token();
            let right = Box::new(self.parse_binary_expression(precedence));
            left = ExpressionSyntax::Binary(BinaryExpressionSyntax {
                left: Box::new(left),
                operator_token,
                right,
            });
        }

        left
    }

    fn parse_primary_expression(&mut self) -> ExpressionSyntax {
        match self.current_kind() {
            SyntaxKind::OpenParenthesisToken => {
                let open_parenthesis_token = self.match_token(SyntaxKind::OpenParenthesisToken);
                let expression = Box::new(self.parse_expression());
                let close_parenthesis_token = self.match_token(SyntaxKind::CloseParenthesisToken);
                ExpressionSyntax::Parenthesized(ParenthesizedExpressionSyntax {
                    open_parenthesis_token,
                    expression,
                    close_parenthesis_token,
                })
            }
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword => {
                let keyword_token = self.next_token();
                ExpressionSyntax::Literal(LiteralExpressionSyntax {
                    value: Some(Object::Bool(
                        keyword_token.kind() == SyntaxKind::TrueKeyword,
                    )),
                    literal_token: keyword_token,
                })
            }
            _ => {
                let number_token = self.match_token(SyntaxKind::NumberToken);
                ExpressionSyntax::Literal(LiteralExpressionSyntax {
                    value: number_token.value().cloned(),
                    literal_token: number_token,
                })
            }
        }
    }
}
