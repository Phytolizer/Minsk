namespace Minsk.CodeAnalysis.Binding;

internal enum BoundNodeKind
{
    BlockStatement,
    ExpressionStatement,
    VariableDeclaration,

    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
    AssignmentExpression
}
