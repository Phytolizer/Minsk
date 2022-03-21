use crate::object::Object;

use super::binding::node::expression::binary::BoundBinaryExpression;
use super::binding::node::expression::literal::BoundLiteralExpression;
use super::binding::node::expression::unary::BoundUnaryExpression;
use super::binding::node::expression::BoundExpression;
use super::binding::node::operator::binary::kind::BoundBinaryOperatorKind;
use super::binding::node::operator::unary::kind::BoundUnaryOperatorKind;

pub(crate) struct Evaluator {
    root: BoundExpression,
}

impl Evaluator {
    pub(crate) fn new(root: BoundExpression) -> Self {
        Self { root }
    }

    pub(crate) fn evaluate(self) -> Object {
        Self::evaluate_expression(self.root)
    }

    fn evaluate_expression(root: BoundExpression) -> Object {
        match root {
            BoundExpression::Binary(b) => Self::evaluate_binary_expression(b),
            BoundExpression::Literal(lit) => Self::evaluate_literal_expression(lit),
            BoundExpression::Unary(u) => Self::evaluate_unary_expression(u),
        }
    }

    fn evaluate_binary_expression(root: BoundBinaryExpression) -> Object {
        let left = Self::evaluate_expression(*root.left);
        let right = Self::evaluate_expression(*root.right);
        // unwrap: safe bc the binder checked the types for us
        match root.operator.kind {
            BoundBinaryOperatorKind::Addition => {
                Object::Number(left.as_number().unwrap() + right.as_number().unwrap())
            }
            BoundBinaryOperatorKind::Subtraction => {
                Object::Number(left.as_number().unwrap() - right.as_number().unwrap())
            }
            BoundBinaryOperatorKind::Multiplication => {
                Object::Number(left.as_number().unwrap() * right.as_number().unwrap())
            }
            BoundBinaryOperatorKind::Division => {
                Object::Number(left.as_number().unwrap() / right.as_number().unwrap())
            }
            BoundBinaryOperatorKind::LogicalAnd => {
                Object::Bool(left.try_into_bool().unwrap() && right.try_into_bool().unwrap())
            }
            BoundBinaryOperatorKind::LogicalOr => {
                Object::Bool(left.try_into_bool().unwrap() || right.try_into_bool().unwrap())
            }
            BoundBinaryOperatorKind::Equality => Object::Bool(left == right),
            BoundBinaryOperatorKind::Inequality => Object::Bool(left != right),
        }
    }

    fn evaluate_literal_expression(root: BoundLiteralExpression) -> Object {
        root.value
    }

    fn evaluate_unary_expression(root: BoundUnaryExpression) -> Object {
        let operand = Self::evaluate_expression(*root.operand);
        match root.operator.kind {
            BoundUnaryOperatorKind::Identity => operand,
            BoundUnaryOperatorKind::Negation => Object::Number(-operand.as_number().unwrap()),
            BoundUnaryOperatorKind::LogicalNegation => {
                Object::Bool(!operand.try_into_bool().unwrap())
            }
        }
    }
}
