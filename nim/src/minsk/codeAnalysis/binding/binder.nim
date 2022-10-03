import std/strformat
import std/options

import minsk/minskObject
import minsk/codeAnalysis/syntax/[
  expressionSyntax,
  syntaxKind,
  syntaxToken,
]
import minsk/codeAnalysis/syntax/expressions/[
  binaryExpressionSyntax,
  literalExpressionSyntax,
  parenthesizedExpressionSyntax,
  unaryExpressionSyntax,
]

import boundExpression
import expressions/[
  boundBinaryExpression,
  boundBinaryOperatorKind,
  boundLiteralExpression,
  boundUnaryExpression,
  boundUnaryOperatorKind,
]

type
  Binder* = object
    mDiagnostics: seq[string]

proc newBinder*(): Binder =
  result.mDiagnostics = @[]

proc diagnostics*(self: Binder): seq[string] =
  self.mDiagnostics

proc bindExpression*(binder: var Binder, syntax: ExpressionSyntax): BoundExpression

proc bindBinaryOperatorKind(
  kind: SyntaxKind,
  leftType: MinskObjectKind,
  rightType: MinskObjectKind
): Option[BoundBinaryOperatorKind] =
  if leftType != mokInteger or rightType != mokInteger:
    return none[BoundBinaryOperatorKind]()

  case kind
  of SyntaxKind.PlusToken:
    BoundBinaryOperatorKind.Addition.some
  of SyntaxKind.MinusToken:
    BoundBinaryOperatorKind.Subtraction.some
  of SyntaxKind.StarToken:
    BoundBinaryOperatorKind.Multiplication.some
  of SyntaxKind.SlashToken:
    BoundBinaryOperatorKind.Division.some
  else:
    raiseAssert fmt"Unexpected binary operator {kind}"

proc bindBinaryExpression(binder: var Binder, syntax: BinaryExpressionSyntax): BoundExpression =
  let boundLeft = binder.bindExpression(syntax.left)
  let boundRight = binder.bindExpression(syntax.right)
  let boundOperatorKind = bindBinaryOperatorKind(
    syntax.operatorToken.kind,
    boundLeft.objectKind,
    boundRight.objectKind
  )
  if boundOperatorKind.isNone:
    binder.mDiagnostics.add(
      fmt"Operator '{syntax.operatorToken.text}' is " &
      fmt"not defined for types '{boundLeft.objectKind}' " &
      fmt"and '{boundRight.objectKind}'"
    )
    return boundLeft

  return newBoundBinaryExpression(boundLeft, boundOperatorKind.get, boundRight)

proc bindLiteralExpression(binder: var Binder, syntax: LiteralExpressionSyntax): BoundExpression =
  newBoundLiteralExpression(syntax.literalToken.value)

proc bindParenthesizedExpression(binder: var Binder, syntax: ParenthesizedExpressionSyntax): BoundExpression =
  binder.bindExpression(syntax.expression)

proc bindUnaryOperatorKind(
  kind: SyntaxKind,
  operandType: MinskObjectKind
): Option[BoundUnaryOperatorKind] =
  if operandType != mokInteger:
    return none[BoundUnaryOperatorKind]()

  case kind
  of SyntaxKind.PlusToken:
    BoundUnaryOperatorKind.Identity.some
  of SyntaxKind.MinusToken:
    BoundUnaryOperatorKind.Negation.some
  else:
    raiseAssert fmt"Unexpected unary operator {kind}"

proc bindUnaryExpression(binder: var Binder, syntax: UnaryExpressionSyntax): BoundExpression =
  let boundOperand = binder.bindExpression(syntax.operand)
  let boundOperatorKind = bindUnaryOperatorKind(
    syntax.operatorToken.kind,
    boundOperand.objectKind
  )
  if boundOperatorKind.isNone:
    binder.mDiagnostics.add(
      fmt"Operator '{syntax.operatorToken.text}' is " &
      fmt"not defined for type '{boundOperand.objectKind}'"
    )
    return boundOperand

  return newBoundUnaryExpression(boundOperatorKind.get, boundOperand)

proc bindExpression*(binder: var Binder, syntax: ExpressionSyntax): BoundExpression =
  case syntax.kind
  of SyntaxKind.BinaryExpression:
    binder.bindBinaryExpression(syntax.BinaryExpressionSyntax)
  of SyntaxKind.LiteralExpression:
    binder.bindLiteralExpression(syntax.LiteralExpressionSyntax)
  of SyntaxKind.ParenthesizedExpression:
    binder.bindParenthesizedExpression(syntax.ParenthesizedExpressionSyntax)
  of SyntaxKind.UnaryExpression:
    binder.bindUnaryExpression(syntax.UnaryExpressionSyntax)
  else:
    raiseAssert fmt"Unexpected syntax {syntax.kind}"
