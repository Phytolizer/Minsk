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
    return SyntaxToken(
      kind: SyntaxKind.EndOfFileToken,
      position: lexer.position,
      text: "",
      value: moNull(),
    )
  let start = lexer.position
  if lexer.current.isDigit:
    while lexer.current.isDigit:
      lexer.position += 1
    let text = lexer.text[start..<lexer.position]
    var value = moNull()
    try:
      value = moInteger(text.parseInt())
    except ValueError:
      # TODO: handle error
      discard
    return SyntaxToken(
      kind: SyntaxKind.NumberToken,
      position: start,
      text: text,
      value: value,
    )
  elif lexer.current.isSpaceAscii:
    while lexer.current.isSpaceAscii:
      lexer.position += 1
    let text = lexer.text[start..<lexer.position]
    return SyntaxToken(
      kind: SyntaxKind.WhitespaceToken,
      position: start,
      text: text,
      value: moNull(),
    )
  else:
    case lexer.current:
    of '+':
      lexer.position += 1
      return SyntaxToken(
        kind: SyntaxKind.PlusToken,
        position: start,
        text: "+",
        value: moNull(),
      )
    of '-':
      lexer.position += 1
      return SyntaxToken(
        kind: SyntaxKind.MinusToken,
        position: start,
        text: "-",
        value: moNull(),
      )
    of '*':
      lexer.position += 1
      return SyntaxToken(
        kind: SyntaxKind.StarToken,
        position: start,
        text: "*",
        value: moNull(),
      )
    of '/':
      lexer.position += 1
      return SyntaxToken(
        kind: SyntaxKind.SlashToken,
        position: start,
        text: "/",
        value: moNull(),
      )
    of '(':
      lexer.position += 1
      return SyntaxToken(
        kind: SyntaxKind.OpenParenthesisToken,
        position: start,
        text: "(",
        value: moNull(),
      )
    of ')':
      lexer.position += 1
      return SyntaxToken(
        kind: SyntaxKind.CloseParenthesisToken,
        position: start,
        text: ")",
        value: moNull(),
      )
    else:
      lexer.position += 1
      return SyntaxToken(
        kind: SyntaxKind.BadToken,
        position: start,
        text: lexer.text[start..<lexer.position],
        value: moNull(),
      )
