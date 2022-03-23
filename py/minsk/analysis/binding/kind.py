from enum import Enum, auto


class BoundNodeKind(Enum):
    AssignmentExpression = auto()
    BinaryExpression = auto()
    LiteralExpression = auto()
    UnaryExpression = auto()
    VariableExpression = auto()
