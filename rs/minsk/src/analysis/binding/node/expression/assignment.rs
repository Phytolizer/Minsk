use crate::object::ObjectKind;

use super::BoundExpression;

#[derive(Debug)]
pub(crate) struct BoundAssignmentExpression {
    pub(crate) name: String,
    pub(crate) expression: Box<BoundExpression>,
}

impl BoundAssignmentExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        self.expression.ty()
    }
}
