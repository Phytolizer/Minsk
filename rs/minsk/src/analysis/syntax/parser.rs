use crate::analysis::diagnostic::Diagnostic;
use crate::analysis::diagnostic_bag::DiagnosticBag;
use crate::analysis::text::source_text::SourceText;
use crate::object::Object;

use super::facts;
use super::kind::SyntaxKind;
use super::lexer::Lexer;
use super::node::expression::assignment::AssignmentExpressionSyntax;
use super::node::expression::binary::BinaryExpressionSyntax;
use super::node::expression::literal::LiteralExpressionSyntax;
use super::node::expression::name::NameExpressionSyntax;
use super::node::expression::parenthesized::ParenthesizedExpressionSyntax;
use super::node::expression::unary::UnaryExpressionSyntax;
use super::node::expression::ExpressionSyntax;
use super::node::unit::CompilationUnitSyntax;
use super::node::SyntaxNode;
use super::token::SyntaxToken;

pub(crate) struct Parser {
    tokens: Vec<SyntaxToken>,
    diagnostics: DiagnosticBag,
    position: usize,
    text: SourceText,
}

impl Parser {
    pub(crate) fn new(text: SourceText) -> Self {
        let lex = Lexer::new(&text);
        let (tokens, diagnostics) = lex.into_tokens_and_diagnostics();
        Self {
            tokens,
            diagnostics,
            position: 0,
            text,
        }
    }

    fn peek(&self, offset: usize) -> SyntaxToken {
        let index = self.position + offset;
        if index < self.tokens.len() {
            return self.tokens[index].clone();
        }

        let last_position = if !self.tokens.is_empty() {
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

    pub(crate) fn parse_compilation_unit(&mut self) -> CompilationUnitSyntax {
        CompilationUnitSyntax::new(
            self.parse_expression(),
            self.match_token(SyntaxKind::EndOfFileToken),
        )
    }

    fn parse_expression(&mut self) -> ExpressionSyntax {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> ExpressionSyntax {
        if self.current_kind() != SyntaxKind::IdentifierToken
            || self.peek_kind(1) != SyntaxKind::EqualsToken
        {
            self.parse_binary_expression(0)
        } else {
            let identifier_token = self.next_token();
            let equals_token = self.next_token();
            let expression = Box::new(self.parse_assignment_expression());
            ExpressionSyntax::Assignment(AssignmentExpressionSyntax {
                identifier_token,
                equals_token,
                expression,
            })
        }
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
            SyntaxKind::OpenParenthesisToken => self.parse_parenthesized_expression(),
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword => self.parse_boolean_literal(),
            SyntaxKind::NumberToken => self.parse_number_literal(),
            _ => self.parse_name_expression(),
        }
    }

    fn parse_number_literal(&mut self) -> ExpressionSyntax {
        let number_token = self.match_token(SyntaxKind::NumberToken);
        ExpressionSyntax::Literal(LiteralExpressionSyntax {
            value: number_token.value().cloned(),
            literal_token: number_token,
        })
    }

    fn parse_name_expression(&mut self) -> ExpressionSyntax {
        let identifier_token = self.match_token(SyntaxKind::IdentifierToken);
        ExpressionSyntax::Name(NameExpressionSyntax { identifier_token })
    }

    fn parse_boolean_literal(&mut self) -> ExpressionSyntax {
        let is_true = self.current_kind() == SyntaxKind::TrueKeyword;
        let keyword_token = if is_true {
            self.match_token(SyntaxKind::TrueKeyword)
        } else {
            self.match_token(SyntaxKind::FalseKeyword)
        };
        ExpressionSyntax::Literal(LiteralExpressionSyntax {
            literal_token: keyword_token,
            value: Some(Object::Bool(is_true)),
        })
    }

    fn parse_parenthesized_expression(&mut self) -> ExpressionSyntax {
        let open_parenthesis_token = self.match_token(SyntaxKind::OpenParenthesisToken);
        let expression = Box::new(self.parse_expression());
        let close_parenthesis_token = self.match_token(SyntaxKind::CloseParenthesisToken);
        ExpressionSyntax::Parenthesized(ParenthesizedExpressionSyntax {
            open_parenthesis_token,
            expression,
            close_parenthesis_token,
        })
    }

    pub(crate) fn unpack(self) -> (SourceText, Vec<Diagnostic>) {
        (self.text, self.diagnostics.into_iter().collect())
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::syntax::asserting_enumerator::AssertingEnumerator;
    use crate::analysis::syntax::facts;
    use crate::analysis::syntax::kind::SyntaxKind;
    use crate::analysis::syntax::node::expression::ExpressionSyntax;
    use crate::analysis::syntax::tree::SyntaxTree;

    fn parse_expression(text: &str) -> ExpressionSyntax {
        SyntaxTree::parse(text).root.expression
    }

    #[test]
    fn binary_expression_honors_precedence() {
        for op1 in facts::get_binary_operator_kinds() {
            for op2 in facts::get_binary_operator_kinds() {
                let op1_precedence = facts::binary_operator_precedence(op1);
                let op2_precedence = facts::binary_operator_precedence(op2);
                let op1_text = facts::get_text(op1).unwrap();
                let op2_text = facts::get_text(op2).unwrap();
                let text = format!("a {} b {} c", op1_text, op2_text);
                let expression = parse_expression(&text);

                if op1_precedence >= op2_precedence {
                    let mut e = AssertingEnumerator::new(&expression);

                    e.assert_node(SyntaxKind::BinaryExpression);
                    e.assert_node(SyntaxKind::BinaryExpression);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "a");
                    e.assert_token(op1, op1_text);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "b");
                    e.assert_token(op2, op2_text);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "c");
                    e.assert_at_end();
                } else {
                    let mut e = AssertingEnumerator::new(&expression);

                    e.assert_node(SyntaxKind::BinaryExpression);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "a");
                    e.assert_token(op1, op1_text);
                    e.assert_node(SyntaxKind::BinaryExpression);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "b");
                    e.assert_token(op2, op2_text);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "c");
                    e.assert_at_end();
                }
            }
        }
    }

    #[test]
    fn unary_expression_honors_precedence() {
        for unary in facts::get_unary_operator_kinds() {
            for binary in facts::get_binary_operator_kinds() {
                let unary_precedence = facts::unary_operator_precedence(unary);
                let binary_precedence = facts::binary_operator_precedence(binary);
                let unary_text = facts::get_text(unary).unwrap();
                let binary_text = facts::get_text(binary).unwrap();
                let text = format!("{} a {} b", unary_text, binary_text);
                let expression = parse_expression(&text);

                if unary_precedence >= binary_precedence {
                    let mut e = AssertingEnumerator::new(&expression);

                    e.assert_node(SyntaxKind::BinaryExpression);
                    e.assert_node(SyntaxKind::UnaryExpression);
                    e.assert_token(unary, unary_text);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "a");
                    e.assert_token(binary, binary_text);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "b");
                    e.assert_at_end();
                } else {
                    let mut e = AssertingEnumerator::new(&expression);

                    e.assert_node(SyntaxKind::UnaryExpression);
                    e.assert_token(unary, unary_text);
                    e.assert_node(SyntaxKind::BinaryExpression);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "a");
                    e.assert_token(binary, binary_text);
                    e.assert_node(SyntaxKind::NameExpression);
                    e.assert_token(SyntaxKind::IdentifierToken, "b");
                    e.assert_at_end();
                }
            }
        }
    }
}
