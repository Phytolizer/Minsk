type
  SyntaxKind* {.pure.} = enum
    BadToken
    EndOfFileToken

    WhitespaceToken
    NumberToken
    IdentifierToken

    PlusToken
    MinusToken
    StarToken
    SlashToken
    OpenParenthesisToken
    CloseParenthesisToken
    BangToken
    AmpersandAmpersandToken
    PipePipeToken
    EqualsEqualsToken
    BangEqualsToken

    TrueKeyword
    FalseKeyword

    LiteralExpression
    BinaryExpression
    ParenthesizedExpression
    UnaryExpression
