import std/strformat
import std/strutils

import minsk/minskObject

import syntaxFacts
import syntaxKind
import syntaxToken

type
  Lexer* = object
    text: string
    position: int
    mDiagnostics: seq[string]

proc newLexer*(text: string): Lexer =
  result.text = text
  result.position = 0
  result.mDiagnostics = @[]

proc peek(lexer: Lexer, offset: int): char =
  let index = lexer.position + offset
  if index >= lexer.text.len:
    return '\0'
  lexer.text[index]

proc current(lexer: Lexer): char =
  lexer.peek(0)

proc nextToken*(lexer: var Lexer): SyntaxToken =
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
      lexer.mDiagnostics.add(fmt"The number {text} isn't a valid int.")
    kind = SyntaxKind.NumberToken
  elif lexer.current.isSpaceAscii:
    while lexer.current.isSpaceAscii:
      lexer.position += 1
    text = lexer.text[start..<lexer.position]
    kind = SyntaxKind.WhitespaceToken
  elif lexer.current.isAlphaAscii:
    while lexer.current.isAlphaAscii:
      lexer.position += 1
    text = lexer.text[start..<lexer.position]
    kind = keywordKind(text)
  else:
    case lexer.current
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
    of '!':
      if lexer.peek(1) == '=':
        lexer.position += 2
        kind = SyntaxKind.BangEqualsToken
      else:
        lexer.position += 1
        kind = SyntaxKind.BangToken
    of '=':
      if lexer.peek(1) == '=':
        lexer.position += 2
        kind = SyntaxKind.EqualsEqualsToken
    of '&':
      if lexer.peek(1) == '&':
        lexer.position += 2
        kind = SyntaxKind.AmpersandAmpersandToken
    of '|':
      if lexer.peek(1) == '|':
        lexer.position += 2
        kind = SyntaxKind.PipePipeToken
    else:
      discard

  if kind == SyntaxKind.BadToken:
    lexer.mDiagnostics.add(fmt"ERROR: Bad character input: '{lexer.current}'")
    lexer.position += 1

  if text.len == 0:
    text = lexer.text[start..<lexer.position]

  return newToken(kind, start, text, value)

proc diagnostics*(lexer: Lexer): seq[string] =
  lexer.mDiagnostics
