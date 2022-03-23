namespace Minsk.CodeAnalysis.Binding;

internal enum BoundNodeKind
{
    BlockStatement,
    ExpressionStatement,

    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
    AssignmentExpression
}
