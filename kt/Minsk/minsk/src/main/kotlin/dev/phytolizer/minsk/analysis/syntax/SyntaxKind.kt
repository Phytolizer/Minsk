package dev.phytolizer.minsk.analysis.syntax

enum class SyntaxKind {
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
    BangToken,
    AmpersandAmpersandToken,
    PipePipeToken,

    TrueKeyword,
    FalseKeyword,

    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    ParenthesizedExpression,
}
