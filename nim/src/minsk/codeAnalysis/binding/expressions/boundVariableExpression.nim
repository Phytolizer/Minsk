import minsk/minskObject
import minsk/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]
import minsk/codeAnalysis/variableSymbol

type
  BoundVariableExpression* = ref object of BoundExpression
    variable*: VariableSymbol

proc newBoundVariableExpression*(variable: VariableSymbol): BoundVariableExpression =
  new(result)
  result.variable = variable

method kind*(self: BoundVariableExpression): BoundNodeKind =
  BoundNodeKind.VariableExpression

method ty*(self: BoundVariableExpression): MinskObjectKind =
  self.variable.ty
