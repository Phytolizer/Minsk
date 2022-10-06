import minsk/minskObject
import minsk/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]

type
  BoundVariableExpression* = ref object of BoundExpression
    name*: string
    mTy*: MinskObjectKind

proc newBoundVariableExpression*(
  name: string,
  ty: MinskObjectKind
): BoundVariableExpression =
  new(result)
  result.name = name
  result.mTy = ty

method kind*(self: BoundVariableExpression): BoundNodeKind =
  BoundNodeKind.VariableExpression

method ty*(self: BoundVariableExpression): MinskObjectKind =
  self.mTy
