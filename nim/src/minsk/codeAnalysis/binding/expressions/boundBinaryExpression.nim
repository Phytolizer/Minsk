import minsk/minskObject
import minsk/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]

import boundBinaryOperatorKind

type
  BoundBinaryExpression* = ref object of BoundExpression
    left*: BoundExpression
    operatorKind*: BoundBinaryOperatorKind
    right*: BoundExpression

proc newBoundBinaryExpression*(
  left: BoundExpression,
  operatorKind: BoundBinaryOperatorKind,
  right: BoundExpression
): BoundBinaryExpression =
  new(result)
  result.left = left
  result.operatorKind = operatorKind
  result.right = right

method kind*(self: BoundBinaryExpression): BoundNodeKind =
  BoundNodeKind.BinaryExpression

method objectKind*(self: BoundBinaryExpression): MinskObjectKind =
  self.left.objectKind
