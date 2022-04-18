namespace Minsk.CodeAnalysis.Binding;

internal enum BoundNodeKind
{
    BlockStatement,
    ExpressionStatement,
    IfStatement,
    ForStatement,
    VariableDeclaration,
    WhileStatement,

    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    VariableExpression,
    AssignmentExpression
}
