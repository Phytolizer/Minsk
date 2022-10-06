import minsk/minskObject
import minsk/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]

import boundUnaryOperator

type
  BoundUnaryExpression* = ref object of BoundExpression
    op*: BoundUnaryOperator
    operand*: BoundExpression

proc newBoundUnaryExpression*(
  op: BoundUnaryOperator,
  operand: BoundExpression
): BoundUnaryExpression =
  new(result)
  result.op = op
  result.operand = operand

method kind*(self: BoundUnaryExpression): BoundNodeKind =
  BoundNodeKind.UnaryExpression

method ty*(self: BoundUnaryExpression): MinskObjectKind =
  self.op.resultType
