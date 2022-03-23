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
    EqualsToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    OpenBraceToken,
    CloseBraceToken,

    FalseKeyword,
    LetKeyword,
    TrueKeyword,
    VarKeyword,

    // Syntax nodes
    CompilationUnit,

    BlockStatement,
    ExpressionStatement,
    VariableDeclaration,

    LiteralExpression,
    BinaryExpression,
    ParenthesizedExpression,
    UnaryExpression,
    NameExpression,
    AssignmentExpression
}
