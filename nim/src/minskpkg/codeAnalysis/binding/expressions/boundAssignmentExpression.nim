import minskpkg/minskObject

import minskpkg/codeAnalysis/binding/[
  boundExpression,
  boundNodeKind,
]
import minskpkg/codeAnalysis/variableSymbol

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
