import minsk/minskObject

import minsk/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]

type
  BoundAssignmentExpression* = ref object of BoundExpression
    name*: string
    expression*: BoundExpression

proc newBoundAssignmentExpression*(
  name: string,
  expression: BoundExpression
): BoundAssignmentExpression =
  new(result)
  result.name = name
  result.expression = expression

method kind*(self: BoundAssignmentExpression): BoundNodeKind =
  BoundNodeKind.AssignmentExpression

method ty*(self: BoundAssignmentExpression): MinskObjectKind =
  self.expression.ty
