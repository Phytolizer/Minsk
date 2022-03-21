use crate::object::ObjectKind;

#[derive(Debug)]
pub(crate) struct BoundVariableExpression {
    pub(crate) name: String,
    pub(crate) ty: ObjectKind,
}

impl BoundVariableExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        self.ty
    }
}
