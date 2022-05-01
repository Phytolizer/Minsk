type
  SyntaxKind* {.pure.} = enum
    BadToken
    EndOfFileToken

    WhitespaceToken
    NumberToken

    PlusToken
    MinusToken
    StarToken
    SlashToken
    OpenParenthesisToken
    CloseParenthesisToken
