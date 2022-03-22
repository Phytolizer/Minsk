package dev.phytolizer.minsk

enum class SyntaxKind {
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

    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    ParenthesizedExpression,
}
