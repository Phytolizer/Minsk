use crate::analysis::variable_symbol::VariableSymbol;
use crate::object::ObjectKind;

#[derive(Debug)]
pub(crate) struct BoundVariableExpression {
    pub(crate) variable: VariableSymbol,
}

impl BoundVariableExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        self.variable.ty
    }
}
