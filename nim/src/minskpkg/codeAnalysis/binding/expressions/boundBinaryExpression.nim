import minskpkg/minskObject
import minskpkg/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]

import boundBinaryOperator

type
  BoundBinaryExpression* = ref object of BoundExpression
    left*: BoundExpression
    op*: BoundBinaryOperator
    right*: BoundExpression

proc newBoundBinaryExpression*(
  left: BoundExpression,
  op: BoundBinaryOperator,
  right: BoundExpression
): BoundBinaryExpression =
  new(result)
  result.left = left
  result.op = op
  result.right = right

method kind*(self: BoundBinaryExpression): BoundNodeKind =
  BoundNodeKind.BinaryExpression

method ty*(self: BoundBinaryExpression): MinskObjectKind =
  self.op.resultType
