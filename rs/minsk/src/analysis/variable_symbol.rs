use crate::object::ObjectKind;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableSymbol {
    pub(crate) name: String,
    pub(crate) ty: ObjectKind,
}
