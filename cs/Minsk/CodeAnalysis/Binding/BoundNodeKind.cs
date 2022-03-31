namespace Minsk.CodeAnalysis.Binding;

internal enum BoundNodeKind
{
    BlockStatement,
    ExpressionStatement,
    IfStatement,
    VariableDeclaration,
    WhileStatement,

    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
    AssignmentExpression
}
