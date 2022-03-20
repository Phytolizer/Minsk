namespace Minsk.CodeAnalysis.Syntax;

public enum SyntaxKind
{
    BadToken,
    EndOfFileToken,

    WhitespaceToken,
    NumberToken,

    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    OpenParenthesisToken,
    CloseParenthesisToken,

    // Syntax nodes
    LiteralExpression,
    BinaryExpression,
    ParenthesizedExpression,
    UnaryExpression
}