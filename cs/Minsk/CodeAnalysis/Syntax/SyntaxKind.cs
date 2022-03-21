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