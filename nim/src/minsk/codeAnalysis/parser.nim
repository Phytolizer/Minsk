import std/strformat

import expressions/binaryExpressionSyntax
import expressions/literalExpressionSyntax
import expressions/parenthesizedExpressionSyntax
import expressions/unaryExpressionSyntax
import expressionSyntax
import lexer
import macros
import minskObject
import syntaxFacts
import syntaxKind
import syntaxToken

type
  Parser* = object
    tokens: seq[SyntaxToken]
    position: int
    mDiagnostics: seq[string]

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
  result.mDiagnostics = lexer.diagnostics

proc diagnostics*(parser: Parser): seq[string] =
  parser.mDiagnostics

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
    parser.mDiagnostics.add(fmt"ERROR: Unexpected token <{result.kind}>, expected <{kind}>")
    result = newToken(kind, result.position, "", moNull())

proc parseExpression(parser: var Parser): ExpressionSyntax

proc parsePrimaryExpression(parser: var Parser): ExpressionSyntax =
  case parser.current.kind
  of SyntaxKind.OpenParenthesisToken:
    let left = parser.nextToken()
    let expression = parser.parseExpression()
    let right = parser.matchToken(SyntaxKind.CloseParenthesisToken)
    return newParenthesizedExpressionSyntax(left, expression, right)
  else:
    let numberToken = parser.matchToken(SyntaxKind.NumberToken)
    return newLiteralExpressionSyntax(numberToken)

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

proc parseExpression(parser: var Parser): ExpressionSyntax =
  parser.parseBinaryExpression()

type
  SyntaxTree* = object
    diagnostics*: seq[string]
    root*: ExpressionSyntax
    endOfFileToken*: SyntaxToken

proc newSyntaxTree*(
  diagnostics: seq[string],
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
