use crate::analysis::binding::node::operator::binary::BoundBinaryOperator;
use crate::object::ObjectKind;

use super::BoundExpression;

#[derive(Debug)]
pub(crate) struct BoundBinaryExpression {
    pub(crate) left: Box<BoundExpression>,
    pub(crate) operator: &'static BoundBinaryOperator,
    pub(crate) right: Box<BoundExpression>,
}

impl BoundBinaryExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        self.operator.result_type
    }
}
