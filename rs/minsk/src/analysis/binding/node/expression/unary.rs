use crate::analysis::binding::node::operator::unary::BoundUnaryOperator;
use crate::object::ObjectKind;

use super::BoundExpression;

#[derive(Debug)]
pub(crate) struct BoundUnaryExpression {
    pub(crate) operator: &'static BoundUnaryOperator,
    pub(crate) operand: Box<BoundExpression>,
}

impl BoundUnaryExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        self.operator.result_type
    }
}
