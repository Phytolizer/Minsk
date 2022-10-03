import std/strformat

import expressions/binaryExpressionSyntax
import expressions/literalExpressionSyntax
import expressionSyntax
import lexer
import macros
import minskObject
import syntaxKind
import syntaxToken
import syntaxTree

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

proc parsePrimaryExpression(parser: var Parser): ExpressionSyntax =
  let numberToken = parser.matchToken(SyntaxKind.NumberToken)
  return newLiteralExpressionSyntax(numberToken)

proc parseExpression(parser: var Parser): ExpressionSyntax =
  result = parser.parsePrimaryExpression()

  while parser.current.kind in {SyntaxKind.PlusToken, SyntaxKind.MinusToken}:
    let operatorToken = parser.nextToken()
    let right = parser.parsePrimaryExpression()
    result = newBinaryExpressionSyntax(result, operatorToken, right)

proc parse*(parser: var Parser): SyntaxTree =
  let expression = parser.parseExpression()
  let endOfFileToken = parser.matchToken(SyntaxKind.EndOfFileToken)
  return newSyntaxTree(parser.diagnostics, expression, endOfFileToken)
