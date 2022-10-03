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

proc evaluateExpression(evaluator: Evaluator, root: BoundExpression): int =
  case root.kind
  of BoundNodeKind.LiteralExpression:
    let l = root.BoundLiteralExpression
    return l.value.intVal
  of BoundNodeKind.BinaryExpression:
    let b = root.BoundBinaryExpression
    let left = evaluateExpression(evaluator, b.left)
    let right = evaluateExpression(evaluator, b.right)
    case b.operatorKind
    of BoundBinaryOperatorKind.Addition:
      return left + right
    of BoundBinaryOperatorKind.Subtraction:
      return left - right
    of BoundBinaryOperatorKind.Multiplication:
      return left * right
    of BoundBinaryOperatorKind.Division:
      return left div right
  of BoundNodeKind.UnaryExpression:
    let u = root.BoundUnaryExpression
    let operand = evaluateExpression(evaluator, u.operand)
    case u.operatorKind
    of BoundUnaryOperatorKind.Identity:
      return operand
    of BoundUnaryOperatorKind.Negation:
      return -operand

proc evaluate*(evaluator: Evaluator): int =
  evaluator.evaluateExpression(evaluator.root)
