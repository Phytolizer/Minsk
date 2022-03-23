from enum import Enum, auto


class SyntaxKind(Enum):
    BadToken = auto()
    EndOfFileToken = auto()

    WhitespaceToken = auto()
    IdentifierToken = auto()
    NumberToken = auto()

    PlusToken = auto()
    MinusToken = auto()
    StarToken = auto()
    SlashToken = auto()
    OpenParenthesisToken = auto()
    CloseParenthesisToken = auto()

    TrueKeyword = auto()
    FalseKeyword = auto()

    BinaryExpression = auto()
    LiteralExpression = auto()
    ParenthesizedExpression = auto()
    UnaryExpression = auto()
