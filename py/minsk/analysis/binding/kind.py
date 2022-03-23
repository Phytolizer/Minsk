from enum import Enum, auto


class BoundNodeKind(Enum):
    BlockStatement = auto()
    ExpressionStatement = auto()
    VariableDeclaration = auto()

    AssignmentExpression = auto()
    BinaryExpression = auto()
    LiteralExpression = auto()
    UnaryExpression = auto()
    VariableExpression = auto()
