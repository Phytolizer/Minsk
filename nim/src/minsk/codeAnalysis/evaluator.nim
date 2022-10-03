import minsk/minskObject

import binding/expressions/[
  boundBinaryExpression,
  boundBinaryOperator,
  boundLiteralExpression,
  boundUnaryExpression,
  boundUnaryOperator,
]
import binding/[
  boundExpression,
  boundNode,
  boundNodeKind,
]

type
  Evaluator* = object
    root: BoundExpression

proc newEvaluator*(root: BoundExpression): Evaluator =
  result.root = root

proc evaluateExpression(evaluator: Evaluator, root: BoundExpression): MinskObject =
  case root.kind
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

proc evaluate*(evaluator: Evaluator): MinskObject =
  evaluator.evaluateExpression(evaluator.root)
