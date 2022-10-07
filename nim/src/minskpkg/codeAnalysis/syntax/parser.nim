import std/sequtils

import minskpkg/codeAnalysis/diagnostic
import minskpkg/codeAnalysis/diagnosticBag
import minskpkg/macros
import minskpkg/minskObject

import expressions/[
  assignmentExpressionSyntax,
  binaryExpressionSyntax,
  literalExpressionSyntax,
  nameExpressionSyntax,
  parenthesizedExpressionSyntax,
  unaryExpressionSyntax,
]
import expressionSyntax
import lexer
import syntaxFacts
import syntaxKind
import syntaxToken

type
  Parser* = object
    tokens: seq[SyntaxToken]
    position: int
    mDiagnostics: DiagnosticBag

proc newParser*(text: string): Parser =
  var lexer = newLexer(text)
  result.tokens = @[]
  loop:
    let token = lexer.nextToken()
    if token.kind notin {SyntaxKind.BadToken, SyntaxKind.WhitespaceToken}:
      result.tokens.add(token)
    if token.kind == SyntaxKind.EndOfFileToken:
      break
  result.position = 0
  result.mDiagnostics = newDiagnosticBag()
  result.mDiagnostics.add(lexer.diagnostics)

proc diagnostics*(parser: Parser): seq[Diagnostic] =
  parser.mDiagnostics.diagnostics.toSeq

proc peek(parser: Parser, offset: int): SyntaxToken =
  let index = parser.position + offset
  if index >= parser.tokens.len:
    return parser.tokens[^1]
  return parser.tokens[index]

proc current(parser: Parser): SyntaxToken =
  parser.peek(0)

proc nextToken(parser: var Parser): SyntaxToken =
  result = parser.current
  parser.position += 1

proc matchToken(parser: var Parser, kind: SyntaxKind): SyntaxToken =
  result = parser.current
  if result.kind == kind:
    parser.position += 1
  else:
    parser.mDiagnostics.reportUnexpectedToken(result.span, result.kind, kind)
    result = newToken(kind, result.position, "", moNull())

proc parseExpression(parser: var Parser): ExpressionSyntax

proc parseParenthesizedExpression(parser: var Parser): ExpressionSyntax =
  let left = parser.matchToken(SyntaxKind.OpenParenthesisToken)
  let expression = parser.parseExpression()
  let right = parser.matchToken(SyntaxKind.CloseParenthesisToken)
  return newParenthesizedExpressionSyntax(left, expression, right)

proc parseBooleanLiteral(parser: var Parser): ExpressionSyntax =
  let value = parser.current.kind == SyntaxKind.TrueKeyword
  let keywordToken = parser.matchToken(
    if value: SyntaxKind.TrueKeyword
    else: SyntaxKind.FalseKeyword
  )
  return newLiteralExpressionSyntax(keywordToken, moBoolean(value))

proc parseNumberLiteral(parser: var Parser): ExpressionSyntax =
  let numberToken = parser.matchToken(SyntaxKind.NumberToken)
  return newLiteralExpressionSyntax(numberToken)

proc parseNameExpression(parser: var Parser): ExpressionSyntax =
  let identifierToken = parser.matchToken(SyntaxKind.IdentifierToken)
  return newNameExpressionSyntax(identifierToken)

proc parsePrimaryExpression(parser: var Parser): ExpressionSyntax =
  case parser.current.kind
  of SyntaxKind.OpenParenthesisToken:
    parser.parseParenthesizedExpression()
  of SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword:
    parser.parseBooleanLiteral()
  of SyntaxKind.IdentifierToken:
    parser.parseNameExpression()
  else:
    parser.parseNumberLiteral()

proc parseBinaryExpression(parser: var Parser, parentPrecedence: int = 0): ExpressionSyntax =
  let unaryOperatorPrecedence = parser.current.kind.unaryOperatorPrecedence
  if unaryOperatorPrecedence != 0 and unaryOperatorPrecedence >= parentPrecedence:
    let operatorToken = parser.nextToken()
    let operand = parser.parseBinaryExpression(unaryOperatorPrecedence)
    result = newUnaryExpressionSyntax(operatorToken, operand)
  else:
    result = parser.parsePrimaryExpression()

  loop:
    let precedence = parser.current.kind.binaryOperatorPrecedence
    if precedence == 0 or precedence <= parentPrecedence:
      break

    let operatorToken = parser.nextToken()
    let right = parser.parseBinaryExpression(precedence)
    result = newBinaryExpressionSyntax(result, operatorToken, right)

proc parseAssignmentExpression(parser: var Parser): ExpressionSyntax =
  if (
    parser.current.kind == SyntaxKind.IdentifierToken and
    parser.peek(1).kind == SyntaxKind.EqualsToken
  ):
    let identifierToken = parser.matchToken(SyntaxKind.IdentifierToken)
    let equalsToken = parser.matchToken(SyntaxKind.EqualsToken)
    let expression = parser.parseAssignmentExpression()
    return newAssignmentExpressionSyntax(identifierToken, equalsToken, expression)

  return parser.parseBinaryExpression()

proc parseExpression(parser: var Parser): ExpressionSyntax =
  parser.parseAssignmentExpression()

type
  SyntaxTree* = object
    diagnostics*: seq[Diagnostic]
    root*: ExpressionSyntax
    endOfFileToken*: SyntaxToken

proc newSyntaxTree*(
  diagnostics: seq[Diagnostic],
  root: ExpressionSyntax,
  endOfFileToken: SyntaxToken
): SyntaxTree =
  result.diagnostics = diagnostics
  result.root = root
  result.endOfFileToken = endOfFileToken

proc parse*(parser: var Parser): SyntaxTree =
  let expression = parser.parseExpression()
  let endOfFileToken = parser.matchToken(SyntaxKind.EndOfFileToken)
  return newSyntaxTree(parser.diagnostics, expression, endOfFileToken)

proc parse*(text: string): SyntaxTree =
  var parser = newParser(text)
  return parser.parse()
