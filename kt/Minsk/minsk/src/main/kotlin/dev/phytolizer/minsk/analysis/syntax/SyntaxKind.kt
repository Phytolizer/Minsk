package dev.phytolizer.minsk.analysis.syntax

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
