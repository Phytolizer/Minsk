module minsk.code_analysis.syntax.kind;

enum SyntaxKind {
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
    ParenthesizedExpression,
    UnaryExpression,
}
