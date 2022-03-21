use crate::object::Object;
use crate::object::ObjectKind;

#[derive(Debug)]
pub(crate) struct BoundLiteralExpression {
    pub(crate) value: Object,
}

impl BoundLiteralExpression {
    pub(crate) fn ty(&self) -> ObjectKind {
        self.value.kind()
    }
}
