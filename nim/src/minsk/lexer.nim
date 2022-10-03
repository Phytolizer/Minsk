import syntaxKind
import syntaxToken
import minskObject
import std/strutils

type
  Lexer* = object
    text: string
    position: int

func newLexer*(text: string): Lexer =
  result.text = text
  result.position = 0

func current(lexer: Lexer): char =
  if lexer.position >= lexer.text.len:
    return '\0'
  return lexer.text[lexer.position]

func nextToken*(lexer: var Lexer): SyntaxToken =
  if lexer.position >= lexer.text.len:
    return newToken(
      SyntaxKind.EndOfFileToken,
      lexer.position,
      "",
      moNull(),
    )
  let start = lexer.position
  var kind = SyntaxKind.BadToken
  var text = ""
  var value = moNull()
  if lexer.current.isDigit:
    while lexer.current.isDigit:
      lexer.position += 1
    text = lexer.text[start..<lexer.position]
    try:
      value = moInteger(text.parseInt())
    except ValueError:
      # TODO: handle error
      discard
    kind = SyntaxKind.NumberToken
  elif lexer.current.isSpaceAscii:
    while lexer.current.isSpaceAscii:
      lexer.position += 1
    text = lexer.text[start..<lexer.position]
    kind = SyntaxKind.WhitespaceToken
  else:
    case lexer.current:
    of '+':
      lexer.position += 1
      kind = SyntaxKind.PlusToken
    of '-':
      lexer.position += 1
      kind = SyntaxKind.MinusToken
    of '*':
      lexer.position += 1
      kind = SyntaxKind.StarToken
    of '/':
      lexer.position += 1
      kind = SyntaxKind.SlashToken
    of '(':
      lexer.position += 1
      kind = SyntaxKind.OpenParenthesisToken
    of ')':
      lexer.position += 1
      kind = SyntaxKind.CloseParenthesisToken
    else:
      discard

  if kind == SyntaxKind.BadToken:
    lexer.position += 1

  if text.len == 0:
    text = lexer.text[start..<lexer.position]

  return newToken(kind, start, text, value)
