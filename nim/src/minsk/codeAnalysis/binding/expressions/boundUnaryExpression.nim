import minsk/minskObject
import minsk/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]

import boundUnaryOperatorKind

type
  BoundUnaryExpression* = ref object of BoundExpression
    operatorKind*: BoundUnaryOperatorKind
    operand*: BoundExpression

proc newBoundUnaryExpression*(
  operatorKind: BoundUnaryOperatorKind,
  operand: BoundExpression
): BoundUnaryExpression =
  new(result)
  result.operatorKind = operatorKind
  result.operand = operand

method kind*(self: BoundUnaryExpression): BoundNodeKind =
  BoundNodeKind.UnaryExpression

method objectKind*(self: BoundUnaryExpression): MinskObjectKind =
  self.operand.objectKind
