import std/sequtils
import std/strformat
import std/options

import sequtilsExt/sequtilsExt

import minsk/codeAnalysis/diagnostic
import minsk/codeAnalysis/diagnosticBag
import minsk/codeAnalysis/syntax/[
  expressionSyntax,
  syntaxKind,
  syntaxToken,
]
import minsk/codeAnalysis/syntax/expressions/[
  assignmentExpressionSyntax,
  binaryExpressionSyntax,
  literalExpressionSyntax,
  nameExpressionSyntax,
  parenthesizedExpressionSyntax,
  unaryExpressionSyntax,
]
import minsk/codeAnalysis/variableMap
import minsk/codeAnalysis/variableSymbol
import minsk/minskObject

import boundExpression
import expressions/[
  boundAssignmentExpression,
  boundBinaryExpression,
  boundBinaryOperator,
  boundLiteralExpression,
  boundUnaryExpression,
  boundUnaryOperator,
  boundVariableExpression,
]

type
  Binder* = object
    mDiagnostics: DiagnosticBag
    variables: VariableMap

proc newBinder*(variables: VariableMap): Binder =
  result.mDiagnostics = newDiagnosticBag()
  result.variables = variables

proc diagnostics*(self: Binder): seq[Diagnostic] =
  self.mDiagnostics.diagnostics.toSeq

proc bindExpression*(
  binder: var Binder,
  syntax: ExpressionSyntax
): BoundExpression

proc bindAssignmentExpression(
  binder: var Binder,
  syntax: AssignmentExpressionSyntax
): BoundExpression =
  let name = syntax.identifierToken.text
  let boundExpression = binder.bindExpression(syntax.expression)

  let existingVariable = sequtilsExt.find(
    binder.variables.keys,
    proc(v: VariableSymbol): bool =
      v.name == name
  )
  if existingVariable.isSome:
    binder.variables.del(existingVariable.get)

  let variable = newVariableSymbol(name, boundExpression.ty)
  binder.variables[variable] = moNull()
  return newBoundAssignmentExpression(variable, boundExpression)

proc bindBinaryExpression(
  binder: var Binder,
  syntax: BinaryExpressionSyntax
): BoundExpression =
  let boundLeft = binder.bindExpression(syntax.left)
  let boundRight = binder.bindExpression(syntax.right)
  let boundOperator = bindBinaryOperator(
    syntax.operatorToken.kind,
    boundLeft.ty,
    boundRight.ty
  )
  if boundOperator.isNone:
    binder.mDiagnostics.reportUndefinedBinaryOperator(
      syntax.operatorToken.span,
      syntax.operatorToken.text,
      boundLeft.ty,
      boundRight.ty
    )
    return boundLeft

  return newBoundBinaryExpression(boundLeft, boundOperator.get, boundRight)

proc bindLiteralExpression(
  binder: var Binder,
  syntax: LiteralExpressionSyntax
): BoundExpression =
  newBoundLiteralExpression(syntax.value)

proc bindNameExpression(
  binder: var Binder,
  syntax: NameExpressionSyntax
): BoundExpression =
  let name = syntax.identifierToken.text
  let variable = sequtilsExt.find(
    binder.variables.keys,
    proc(v: VariableSymbol): bool = v.name == name
  )
  if variable.isNone:
    binder.mDiagnostics.reportUndefinedName(syntax.identifierToken.span, name)
    return newBoundLiteralExpression(moInteger(0))

  return newBoundVariableExpression(variable.get)

proc bindParenthesizedExpression(
  binder: var Binder,
  syntax: ParenthesizedExpressionSyntax
): BoundExpression =
  binder.bindExpression(syntax.expression)

proc bindUnaryExpression(binder: var Binder,
    syntax: UnaryExpressionSyntax): BoundExpression =
  let boundOperand = binder.bindExpression(syntax.operand)
  let boundOperator = bindUnaryOperator(
    syntax.operatorToken.kind,
    boundOperand.ty
  )
  if boundOperator.isNone:
    binder.mDiagnostics.reportUndefinedUnaryOperator(
      syntax.operatorToken.span,
      syntax.operatorToken.text,
      boundOperand.ty
    )
    return boundOperand

  return newBoundUnaryExpression(boundOperator.get, boundOperand)

proc bindExpression*(binder: var Binder,
    syntax: ExpressionSyntax): BoundExpression =
  case syntax.kind
  of SyntaxKind.AssignmentExpression:
    binder.bindAssignmentExpression(syntax.AssignmentExpressionSyntax)
  of SyntaxKind.BinaryExpression:
    binder.bindBinaryExpression(syntax.BinaryExpressionSyntax)
  of SyntaxKind.LiteralExpression:
    binder.bindLiteralExpression(syntax.LiteralExpressionSyntax)
  of SyntaxKind.NameExpression:
    binder.bindNameExpression(syntax.NameExpressionSyntax)
  of SyntaxKind.ParenthesizedExpression:
    binder.bindParenthesizedExpression(syntax.ParenthesizedExpressionSyntax)
  of SyntaxKind.UnaryExpression:
    binder.bindUnaryExpression(syntax.UnaryExpressionSyntax)
  else:
    raiseAssert fmt"Unexpected syntax {syntax.kind}"
