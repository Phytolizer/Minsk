use crate::analysis::diagnostic::Diagnostic;
use crate::analysis::diagnostic_bag::DiagnosticBag;
use crate::analysis::syntax::node::expression::binary::BinaryExpressionSyntax;
use crate::analysis::syntax::node::expression::literal::LiteralExpressionSyntax;
use crate::analysis::syntax::node::expression::parenthesized::ParenthesizedExpressionSyntax;
use crate::analysis::syntax::node::expression::unary::UnaryExpressionSyntax;
use crate::analysis::syntax::node::expression::ExpressionSyntax;
use crate::analysis::syntax::node::SyntaxNode;
use crate::analysis::syntax::tree::SyntaxTree;
use crate::object::Object;

use super::node::expression::binary::BoundBinaryExpression;
use super::node::expression::literal::BoundLiteralExpression;
use super::node::expression::unary::BoundUnaryExpression;
use super::node::expression::BoundExpression;
use super::node::operator::binary::BoundBinaryOperator;
use super::node::operator::unary::BoundUnaryOperator;

pub(crate) struct Binder {
    diagnostics: DiagnosticBag,
}

impl Binder {
    pub(crate) fn new() -> Self {
        Self {
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub(crate) fn into_expression_and_diagnostics(
        mut self,
        syntax: SyntaxTree,
    ) -> (BoundExpression, Vec<Diagnostic>) {
        (
            self.bind_expression(syntax.root),
            syntax
                .diagnostics
                .into_iter()
                .chain(self.diagnostics.into_iter())
                .collect(),
        )
    }

    fn bind_expression(&mut self, syntax: ExpressionSyntax) -> BoundExpression {
        match syntax {
            ExpressionSyntax::Binary(b) => self.bind_binary_expression(b),
            ExpressionSyntax::Literal(lit) => self.bind_literal_expression(lit),
            ExpressionSyntax::Parenthesized(p) => self.bind_parenthesized_expression(p),
            ExpressionSyntax::Unary(u) => self.bind_unary_expression(u),
        }
    }

    fn bind_binary_expression(&mut self, syntax: BinaryExpressionSyntax) -> BoundExpression {
        let bound_left = self.bind_expression(*syntax.left);
        let bound_right = self.bind_expression(*syntax.right);
        match BoundBinaryOperator::bind(
            syntax.operator_token.kind(),
            bound_left.ty(),
            bound_right.ty(),
        ) {
            Some(operator) => BoundExpression::Binary(BoundBinaryExpression {
                left: Box::new(bound_left),
                operator,
                right: Box::new(bound_right),
            }),
            None => {
                self.diagnostics.report_undefined_binary_operator(
                    syntax.operator_token.span(),
                    syntax.operator_token.text(),
                    bound_left.ty(),
                    bound_right.ty(),
                );
                bound_left
            }
        }
    }

    fn bind_literal_expression(&mut self, syntax: LiteralExpressionSyntax) -> BoundExpression {
        BoundExpression::Literal(BoundLiteralExpression {
            value: syntax.value.unwrap_or(Object::Number(0)),
        })
    }

    fn bind_parenthesized_expression(
        &mut self,
        syntax: ParenthesizedExpressionSyntax,
    ) -> BoundExpression {
        self.bind_expression(*syntax.expression)
    }

    fn bind_unary_expression(&mut self, syntax: UnaryExpressionSyntax) -> BoundExpression {
        let bound_operand = self.bind_expression(*syntax.operand);
        match BoundUnaryOperator::bind(syntax.operator_token.kind(), bound_operand.ty()) {
            Some(operator) => BoundExpression::Unary(BoundUnaryExpression {
                operator,
                operand: Box::new(bound_operand),
            }),
            None => {
                self.diagnostics.report_undefined_unary_operator(
                    syntax.operator_token.span(),
                    syntax.operator_token.text(),
                    bound_operand.ty(),
                );
                bound_operand
            }
        }
    }
}
