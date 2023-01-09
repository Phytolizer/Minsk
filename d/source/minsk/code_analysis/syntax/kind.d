module minsk.code_analysis.syntax.kind;

enum SyntaxKind {
    BadToken,
    EndOfFileToken,
    WhitespaceToken,
    NumberToken,
    IdentifierToken,
    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    OpenParenthesisToken,
    CloseParenthesisToken,

    TrueKeyword,
    FalseKeyword,

    BinaryExpression,
    LiteralExpression,
    ParenthesizedExpression,
    UnaryExpression,
}
