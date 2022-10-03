import minsk/minskObject

import binding/expressions/[
  boundBinaryExpression,
  boundBinaryOperatorKind,
  boundLiteralExpression,
  boundUnaryExpression,
  boundUnaryOperatorKind,
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
    let left = evaluateExpression(evaluator, b.left).intVal
    let right = evaluateExpression(evaluator, b.right).intVal
    case b.operatorKind
    of BoundBinaryOperatorKind.Addition:
      return moInteger(left + right)
    of BoundBinaryOperatorKind.Subtraction:
      return moInteger(left - right)
    of BoundBinaryOperatorKind.Multiplication:
      return moInteger(left * right)
    of BoundBinaryOperatorKind.Division:
      return moInteger(left div right)
  of BoundNodeKind.UnaryExpression:
    let u = root.BoundUnaryExpression
    let operand = evaluateExpression(evaluator, u.operand).intVal
    case u.operatorKind
    of BoundUnaryOperatorKind.Identity:
      return moInteger(operand)
    of BoundUnaryOperatorKind.Negation:
      return moInteger(-operand)

proc evaluate*(evaluator: Evaluator): MinskObject =
  evaluator.evaluateExpression(evaluator.root)
