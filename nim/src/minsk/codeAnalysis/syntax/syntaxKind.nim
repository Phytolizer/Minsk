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

    TrueKeyword
    FalseKeyword

    LiteralExpression
    BinaryExpression
    ParenthesizedExpression
    UnaryExpression
