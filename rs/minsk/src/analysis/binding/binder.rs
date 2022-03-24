use std::collections::HashMap;

use crate::analysis::diagnostic::Diagnostic;
use crate::analysis::diagnostic_bag::DiagnosticBag;
use crate::analysis::syntax::node::expression::assignment::AssignmentExpressionSyntax;
use crate::analysis::syntax::node::expression::binary::BinaryExpressionSyntax;
use crate::analysis::syntax::node::expression::literal::LiteralExpressionSyntax;
use crate::analysis::syntax::node::expression::name::NameExpressionSyntax;
use crate::analysis::syntax::node::expression::parenthesized::ParenthesizedExpressionSyntax;
use crate::analysis::syntax::node::expression::unary::UnaryExpressionSyntax;
use crate::analysis::syntax::node::expression::ExpressionSyntax;
use crate::analysis::syntax::node::SyntaxNode;
use crate::analysis::syntax::tree::SyntaxTree;
use crate::analysis::variable_symbol::VariableSymbol;
use crate::object::Object;

use super::node::expression::assignment::BoundAssignmentExpression;
use super::node::expression::binary::BoundBinaryExpression;
use super::node::expression::literal::BoundLiteralExpression;
use super::node::expression::unary::BoundUnaryExpression;
use super::node::expression::variable::BoundVariableExpression;
use super::node::expression::BoundExpression;
use super::node::operator::binary::BoundBinaryOperator;
use super::node::operator::unary::BoundUnaryOperator;

pub(crate) struct Binder<'v> {
    diagnostics: DiagnosticBag,
    variables: &'v mut HashMap<VariableSymbol, Object>,
}

impl<'v> Binder<'v> {
    pub(crate) fn new(variables: &'v mut HashMap<VariableSymbol, Object>) -> Self {
        Self {
            diagnostics: DiagnosticBag::new(),
            variables,
        }
    }

    pub(crate) fn into_expression_and_diagnostics(
        mut self,
        syntax: &SyntaxTree,
    ) -> (BoundExpression, Vec<Diagnostic>) {
        (
            self.bind_expression(&syntax.root),
            syntax
                .diagnostics
                .iter()
                .cloned()
                .chain(self.diagnostics.into_iter())
                .collect(),
        )
    }

    fn bind_expression(&mut self, syntax: &ExpressionSyntax) -> BoundExpression {
        match syntax {
            ExpressionSyntax::Assignment(a) => self.bind_assignment_expression(a),
            ExpressionSyntax::Binary(b) => self.bind_binary_expression(b),
            ExpressionSyntax::Literal(lit) => self.bind_literal_expression(lit),
            ExpressionSyntax::Parenthesized(p) => self.bind_parenthesized_expression(p),
            ExpressionSyntax::Unary(u) => self.bind_unary_expression(u),
            ExpressionSyntax::Name(n) => self.bind_name_expression(n),
        }
    }

    fn bind_assignment_expression(
        &mut self,
        syntax: &AssignmentExpressionSyntax,
    ) -> BoundExpression {
        let name = &syntax.identifier_token.text;
        let bound_expression = self.bind_expression(&syntax.expression);

        let existing_variable = self.variables.keys().find(|k| &k.name == name);
        if let Some(existing_variable) = existing_variable.cloned() {
            self.variables.remove(&existing_variable);
        }
        let variable = VariableSymbol {
            name: name.to_string(),
            ty: bound_expression.ty(),
        };
        // will be overwritten pretty quickly, so this is a dummy
        let default_value = Object::Number(0);
        self.variables.insert(variable.clone(), default_value);

        BoundExpression::Assignment(BoundAssignmentExpression {
            variable,
            expression: Box::new(bound_expression),
        })
    }

    fn bind_binary_expression(&mut self, syntax: &BinaryExpressionSyntax) -> BoundExpression {
        let bound_left = self.bind_expression(&syntax.left);
        let bound_right = self.bind_expression(&syntax.right);
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

    fn bind_literal_expression(&mut self, syntax: &LiteralExpressionSyntax) -> BoundExpression {
        BoundExpression::Literal(BoundLiteralExpression {
            value: syntax.value.clone().unwrap_or(Object::Number(0)),
        })
    }

    fn bind_name_expression(&mut self, syntax: &NameExpressionSyntax) -> BoundExpression {
        let identifier_span = syntax.identifier_token.span();
        let name = &syntax.identifier_token.text;
        match self.variables.keys().find(|k| &k.name == name).cloned() {
            Some(variable) => BoundExpression::Variable(BoundVariableExpression { variable }),
            None => {
                self.diagnostics
                    .report_undefined_name(identifier_span, name.as_str());
                BoundExpression::Literal(BoundLiteralExpression {
                    value: Object::Number(0),
                })
            }
        }
    }

    fn bind_parenthesized_expression(
        &mut self,
        syntax: &ParenthesizedExpressionSyntax,
    ) -> BoundExpression {
        self.bind_expression(&syntax.expression)
    }

    fn bind_unary_expression(&mut self, syntax: &UnaryExpressionSyntax) -> BoundExpression {
        let bound_operand = self.bind_expression(&syntax.operand);
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
