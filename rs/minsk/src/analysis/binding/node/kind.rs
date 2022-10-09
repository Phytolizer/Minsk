#[allow(clippy::enum_variant_names)]
pub(crate) enum BoundNodeKind {
    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
    AssignmentExpression,
}
