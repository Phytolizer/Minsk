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
    BangToken,
    AmpersandAmpersandToken,
    PipePipeToken,
    EqualsEqualsToken,
    BangEqualsToken,

    TrueKeyword,
    FalseKeyword,

    BinaryExpression,
    LiteralExpression,
    ParenthesizedExpression,
    UnaryExpression,
}
