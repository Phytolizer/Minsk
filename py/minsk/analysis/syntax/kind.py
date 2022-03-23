from enum import Enum, auto


class SyntaxKind(Enum):
    BadToken = auto()
    EndOfFileToken = auto()

    WhitespaceToken = auto()
    NumberToken = auto()

    PlusToken = auto()
    MinusToken = auto()
    StarToken = auto()
    SlashToken = auto()
    OpenParenthesisToken = auto()
    CloseParenthesisToken = auto()

    BinaryExpression = auto()
    LiteralExpression = auto()
    ParenthesizedExpression = auto()
    UnaryExpression = auto()
