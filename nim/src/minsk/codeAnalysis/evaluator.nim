import std/strformat

import syntax/expressions/[
  binaryExpressionSyntax,
  literalExpressionSyntax,
  parenthesizedExpressionSyntax,
  unaryExpressionSyntax,
]
import syntax/[
  expressionSyntax,
  syntaxKind,
  syntaxNode,
]

type
  Evaluator* = object
    root: ExpressionSyntax

proc newEvaluator*(root: ExpressionSyntax): Evaluator =
  result.root = root

proc evaluateExpression(evaluator: Evaluator, root: ExpressionSyntax): int =
  case root.kind
  of SyntaxKind.LiteralExpression:
    let l = root.LiteralExpressionSyntax
    return l.literalToken.value.intVal
  of SyntaxKind.BinaryExpression:
    let b = root.BinaryExpressionSyntax
    let left = evaluateExpression(evaluator, b.left)
    let right = evaluateExpression(evaluator, b.right)
    case b.operatorToken.kind
    of SyntaxKind.PlusToken:
      return left + right
    of SyntaxKind.MinusToken:
      return left - right
    of SyntaxKind.StarToken:
      return left * right
    of SyntaxKind.SlashToken:
      return left div right
    else:
      raiseAssert fmt"Unexpected binary operator {b.operatorToken.kind}"
  of SyntaxKind.ParenthesizedExpression:
    let p = root.ParenthesizedExpressionSyntax
    return evaluateExpression(evaluator, p.expression)
  of SyntaxKind.UnaryExpression:
    let u = root.UnaryExpressionSyntax
    let operand = evaluateExpression(evaluator, u.operand)
    case u.operatorToken.kind
    of SyntaxKind.PlusToken:
      return operand
    of SyntaxKind.MinusToken:
      return -operand
    else:
      raiseAssert fmt"Unexpected unary operator {u.operatorToken.kind}"
  else:
    raiseAssert fmt"Unexpected node {root.kind}"

proc evaluate*(evaluator: Evaluator): int =
  evaluator.evaluateExpression(evaluator.root)
