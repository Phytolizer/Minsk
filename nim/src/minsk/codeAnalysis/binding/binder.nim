import std/sequtils
import std/strformat
import std/options

import minsk/codeAnalysis/diagnostic
import minsk/codeAnalysis/diagnosticBag
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
  boundBinaryOperator,
  boundLiteralExpression,
  boundUnaryExpression,
  boundUnaryOperator,
]

type
  Binder* = object
    mDiagnostics: DiagnosticBag

proc newBinder*(): Binder =
  result.mDiagnostics = newDiagnosticBag()

proc diagnostics*(self: Binder): seq[Diagnostic] =
  self.mDiagnostics.diagnostics.toSeq

proc bindExpression*(
  binder: var Binder,
  syntax: ExpressionSyntax
): BoundExpression

proc bindBinaryExpression(binder: var Binder,
    syntax: BinaryExpressionSyntax): BoundExpression =
  let boundLeft = binder.bindExpression(syntax.left)
  let boundRight = binder.bindExpression(syntax.right)
  let boundOperator = bindBinaryOperator(
    syntax.operatorToken.kind,
    boundLeft.objectKind,
    boundRight.objectKind
  )
  if boundOperator.isNone:
    binder.mDiagnostics.reportUndefinedBinaryOperator(
      syntax.operatorToken.span,
      syntax.operatorToken.text,
      boundLeft.objectKind,
      boundRight.objectKind
    )
    return boundLeft

  return newBoundBinaryExpression(boundLeft, boundOperator.get, boundRight)

proc bindLiteralExpression(binder: var Binder,
    syntax: LiteralExpressionSyntax): BoundExpression =
  newBoundLiteralExpression(syntax.value)

proc bindParenthesizedExpression(binder: var Binder,
    syntax: ParenthesizedExpressionSyntax): BoundExpression =
  binder.bindExpression(syntax.expression)

proc bindUnaryExpression(binder: var Binder,
    syntax: UnaryExpressionSyntax): BoundExpression =
  let boundOperand = binder.bindExpression(syntax.operand)
  let boundOperator = bindUnaryOperator(
    syntax.operatorToken.kind,
    boundOperand.objectKind
  )
  if boundOperator.isNone:
    binder.mDiagnostics.reportUndefinedUnaryOperator(
      syntax.operatorToken.span,
      syntax.operatorToken.text,
      boundOperand.objectKind
    )
    return boundOperand

  return newBoundUnaryExpression(boundOperator.get, boundOperand)

proc bindExpression*(binder: var Binder,
    syntax: ExpressionSyntax): BoundExpression =
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
