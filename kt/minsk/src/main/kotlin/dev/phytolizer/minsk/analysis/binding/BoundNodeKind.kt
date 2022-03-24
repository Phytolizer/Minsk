package dev.phytolizer.minsk.analysis.binding

internal enum class BoundNodeKind {
    BlockStatement,
    ExpressionStatement,
    IfStatement,
    VariableDeclaration,

    AssignmentExpression,
    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
}
