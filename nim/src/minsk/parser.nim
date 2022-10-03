import expressions/binaryExpressionSyntax
import expressions/literalExpressionSyntax
import expressionSyntax
import lexer
import macros
import minskObject
import syntaxKind
import syntaxToken

type
  Parser* = object
    tokens: seq[SyntaxToken]
    position: int

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

proc peek(parser: Parser, offset: int): SyntaxToken =
  let index = parser.position + offset
  if index >= parser.tokens.len:
    return parser.tokens[^1]
  return parser.tokens[index]

proc current(parser: Parser): SyntaxToken =
  peek(parser, 0)

proc nextToken(parser: var Parser): SyntaxToken =
  result = current(parser)
  parser.position += 1

proc matchToken(parser: var Parser, kind: SyntaxKind): SyntaxToken =
  result = current(parser)
  if result.kind == kind:
    parser.position += 1
  else:
    result = newToken(kind, result.position, "", moNull())

proc parsePrimaryExpression(parser: var Parser): ExpressionSyntax =
  let numberToken = matchToken(parser, SyntaxKind.NumberToken)
  return newLiteralExpressionSyntax(numberToken)

proc parse*(parser: var Parser): ExpressionSyntax =
  result = parser.parsePrimaryExpression()

  while parser.current.kind in {SyntaxKind.PlusToken, SyntaxKind.MinusToken}:
    let operatorToken = parser.nextToken()
    let right = parser.parsePrimaryExpression()
    result = newBinaryExpressionSyntax(result, operatorToken, right)
