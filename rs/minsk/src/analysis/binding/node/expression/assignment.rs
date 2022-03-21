use crate::analysis::variable_symbol::VariableSymbol;
use crate::object::ObjectKind;

use super::BoundExpression;

#[derive(Debug)]
pub(crate) struct BoundAssignmentExpression {
    pub(crate) variable: VariableSymbol,
    pub(crate) expression: Box<BoundExpression>,
}

impl BoundAssignmentExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        self.variable.ty
    }
}
