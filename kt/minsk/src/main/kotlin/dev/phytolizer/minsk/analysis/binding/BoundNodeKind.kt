package dev.phytolizer.minsk.analysis.binding

internal enum class BoundNodeKind {
    AssignmentExpression,
    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
}
