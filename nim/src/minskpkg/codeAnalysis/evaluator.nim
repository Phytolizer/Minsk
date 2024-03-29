import minskpkg/minskObject

import binding/expressions/[
  boundAssignmentExpression,
  boundBinaryExpression,
  boundBinaryOperator,
  boundLiteralExpression,
  boundUnaryExpression,
  boundUnaryOperator,
  boundVariableExpression,
]
import binding/[
  boundExpression,
  boundNode,
  boundNodeKind,
]
import variableMap

type
  Evaluator* = object
    root: BoundExpression
    variables: VariableMap

proc newEvaluator*(
  root: BoundExpression,
  variables: VariableMap
): Evaluator =
  result.root = root
  result.variables = variables

proc evaluateExpression(evaluator: var Evaluator, root: BoundExpression): MinskObject =
  case root.kind
  of BoundNodeKind.AssignmentExpression:
    let a = root.BoundAssignmentExpression
    let value = evaluator.evaluateExpression(a.expression)
    evaluator.variables[a.variable] = value
    return value
  of BoundNodeKind.LiteralExpression:
    let l = root.BoundLiteralExpression
    return l.value
  of BoundNodeKind.BinaryExpression:
    let b = root.BoundBinaryExpression
    let left = evaluateExpression(evaluator, b.left)
    let right = evaluateExpression(evaluator, b.right)
    case b.op.kind
    of BoundBinaryOperatorKind.Addition:
      return moInteger(left.intVal + right.intVal)
    of BoundBinaryOperatorKind.Subtraction:
      return moInteger(left.intVal - right.intVal)
    of BoundBinaryOperatorKind.Multiplication:
      return moInteger(left.intVal * right.intVal)
    of BoundBinaryOperatorKind.Division:
      return moInteger(left.intVal div right.intVal)
    of BoundBinaryOperatorKind.LogicalAnd:
      return moBoolean(left.boolVal and right.boolVal)
    of BoundBinaryOperatorKind.LogicalOr:
      return moBoolean(left.boolVal or right.boolVal)
    of BoundBinaryOperatorKind.Equality:
      return moBoolean(left == right)
    of BoundBinaryOperatorKind.Inequality:
      return moBoolean(left != right)
  of BoundNodeKind.UnaryExpression:
    let u = root.BoundUnaryExpression
    let operand = evaluateExpression(evaluator, u.operand)
    case u.op.kind
    of BoundUnaryOperatorKind.Identity:
      return moInteger(operand.intVal)
    of BoundUnaryOperatorKind.Negation:
      return moInteger(-operand.intVal)
    of BoundUnaryOperatorKind.LogicalNegation:
      return moBoolean(not operand.boolVal)
  of BoundNodeKind.VariableExpression:
    let v = root.BoundVariableExpression
    return evaluator.variables[v.variable]

proc evaluate*(evaluator: var Evaluator): MinskObject =
  evaluator.evaluateExpression(evaluator.root)
