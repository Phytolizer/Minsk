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
    TildeToken,
    HatToken,
    AmpersandToken,
    PipeToken,
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
    ForKeyword,
    IfKeyword,
    LetKeyword,
    ToKeyword,
    TrueKeyword,
    VarKeyword,
    WhileKeyword,

    // Syntax nodes
    CompilationUnit,
    ElseClause,

    BlockStatement,
    ExpressionStatement,
    IfStatement,
    ForStatement,
    VariableDeclaration,
    WhileStatement,

    LiteralExpression,
    BinaryExpression,
    ParenthesizedExpression,
    UnaryExpression,
    NameExpression,
    AssignmentExpression,
}
