namespace Minsk.CodeAnalysis.Binding;

internal enum BoundNodeKind
{
    BlockStatement,
    ExpressionStatement,
    IfStatement,
    VariableDeclaration,

    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
    AssignmentExpression
}
