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
    EqualsToken

    TrueKeyword
    FalseKeyword

    LiteralExpression
    BinaryExpression
    ParenthesizedExpression
    UnaryExpression
    NameExpression
    AssignmentExpression
