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
    OpenParenthesisToken,
    CloseParenthesisToken,
    
    TrueKeyword,
    FalseKeyword,

    // Syntax nodes
    LiteralExpression,
    BinaryExpression,
    ParenthesizedExpression,
    UnaryExpression,
}