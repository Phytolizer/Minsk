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
    OpenBraceToken,
    CloseBraceToken,
    BangToken,
    AmpersandAmpersandToken,
    PipePipeToken,
    EqualsEqualsToken,
    BangEqualsToken,
    EqualsToken,

    TrueKeyword,
    FalseKeyword,

    CompilationUnit,

    BlockStatement,
    ExpressionStatement,

    AssignmentExpression,
    BinaryExpression,
    LiteralExpression,
    NameExpression,
    ParenthesizedExpression,
    UnaryExpression,
}
