import minsk/minskObject

import minsk/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]
import minsk/codeAnalysis/variableSymbol

type
  BoundAssignmentExpression* = ref object of BoundExpression
    variable*: VariableSymbol
    expression*: BoundExpression

proc newBoundAssignmentExpression*(
  variable: VariableSymbol,
  expression: BoundExpression
): BoundAssignmentExpression =
  new(result)
  result.variable = variable
  result.expression = expression

method kind*(self: BoundAssignmentExpression): BoundNodeKind =
  BoundNodeKind.AssignmentExpression

method ty*(self: BoundAssignmentExpression): MinskObjectKind =
  self.expression.ty
