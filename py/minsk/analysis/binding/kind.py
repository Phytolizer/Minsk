from enum import Enum, auto


class BoundNodeKind(Enum):
    BinaryExpression = auto()
    LiteralExpression = auto()
    UnaryExpression = auto()
