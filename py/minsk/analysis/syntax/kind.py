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
    BangToken = auto()
    AmpersandAmpersandToken = auto()
    PipePipeToken = auto()
    BangEqualsToken = auto()
    EqualsEqualsToken = auto()
    OpenParenthesisToken = auto()
    CloseParenthesisToken = auto()

    TrueKeyword = auto()
    FalseKeyword = auto()

    AssignmentExpression = auto()
    BinaryExpression = auto()
    LiteralExpression = auto()
    NameExpression = auto()
    ParenthesizedExpression = auto()
    UnaryExpression = auto()
