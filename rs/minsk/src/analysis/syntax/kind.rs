use strum::EnumIter;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, EnumIter)]
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
    EqualsToken,
    OpenParenthesisToken,
    CloseParenthesisToken,

    TrueKeyword,
    FalseKeyword,

    CompilationUnit,

    AssignmentExpression,
    BinaryExpression,
    LiteralExpression,
    NameExpression,
    UnaryExpression,
    ParenthesizedExpression,
}
