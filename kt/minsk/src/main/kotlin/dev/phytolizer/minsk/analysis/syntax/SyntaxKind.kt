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
    LessToken,
    LessOrEqualsToken,
    GreaterToken,
    GreaterOrEqualsToken,

    FalseKeyword,
    LetKeyword,
    TrueKeyword,
    VarKeyword,

    CompilationUnit,

    BlockStatement,
    ExpressionStatement,
    VariableDeclaration,

    AssignmentExpression,
    BinaryExpression,
    LiteralExpression,
    NameExpression,
    ParenthesizedExpression,
    UnaryExpression,
}
