type
  BoundNodeKind* {.pure.} = enum
    AssignmentExpression
    BinaryExpression
    UnaryExpression
    LiteralExpression
    VariableExpression
