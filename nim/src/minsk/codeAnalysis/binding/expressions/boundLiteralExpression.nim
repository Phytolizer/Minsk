import minsk/minskObject
import minsk/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]

type
  BoundLiteralExpression* = ref object of BoundExpression
    value*: MinskObject

proc newBoundLiteralExpression*(value: MinskObject): BoundLiteralExpression =
  new(result)
  result.value = value

method kind*(self: BoundLiteralExpression): BoundNodeKind =
  BoundNodeKind.LiteralExpression

method ty*(self: BoundLiteralExpression): MinskObjectKind =
  self.value.kind
