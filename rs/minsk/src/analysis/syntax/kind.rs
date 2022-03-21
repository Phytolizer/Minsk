#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SyntaxKind {
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
    BangEqualsToken,
    EqualsEqualsToken,
    OpenParenthesisToken,
    CloseParenthesisToken,

    TrueKeyword,
    FalseKeyword,

    BinaryExpression,
    LiteralExpression,
    UnaryExpression,
    ParenthesizedExpression,
}
