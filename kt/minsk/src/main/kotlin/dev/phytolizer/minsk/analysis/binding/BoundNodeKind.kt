package dev.phytolizer.minsk.analysis.binding

internal enum class BoundNodeKind {
    BlockStatement,
    ExpressionStatement,

    AssignmentExpression,
    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
}
