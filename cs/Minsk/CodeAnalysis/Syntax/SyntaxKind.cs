namespace Minsk.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    BadToken,
    EndOfFileToken,

    WhitespaceToken,
    IdentifierToken,
    NumberToken,

    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    BangToken,
    AmpersandAmpersandToken,
    PipePipeToken,
    BangEqualsToken,
    EqualsEqualsToken,
    LessToken,
    GreaterToken,
    LessEqualsToken,
    GreaterEqualsToken,
    EqualsToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    OpenBraceToken,
    CloseBraceToken,

    ElseKeyword,
    FalseKeyword,
    IfKeyword,
    LetKeyword,
    TrueKeyword,
    VarKeyword,

    // Syntax nodes
    CompilationUnit,
    ElseClause,

    BlockStatement,
    ExpressionStatement,
    IfStatement,
    VariableDeclaration,

    LiteralExpression,
    BinaryExpression,
    ParenthesizedExpression,
    UnaryExpression,
    NameExpression,
    AssignmentExpression
}
