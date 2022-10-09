import std/sequtils
import std/strformat
import std/strutils
import std/unittest

import minskpkg/codeAnalysis/syntax/[
  parser,
  syntaxKind,
  syntaxToken,
]

import helpers
import strutilsExt

type
  TestToken = object
    kind: SyntaxKind
    text: string

proc tt(kind: SyntaxKind, text: string): TestToken =
  result.kind = kind
  result.text = text

iterator getTokens(): TestToken =
  const tokens = [
    tt(SyntaxKind.IdentifierToken, "a"),
    tt(SyntaxKind.IdentifierToken, "abc"),
    tt(SyntaxKind.NumberToken, "1"),
    tt(SyntaxKind.NumberToken, "123"),
    tt(SyntaxKind.PlusToken, "+"),
    tt(SyntaxKind.MinusToken, "-"),
    tt(SyntaxKind.StarToken, "*"),
    tt(SyntaxKind.SlashToken, "/"),
    tt(SyntaxKind.BangToken, "!"),
    tt(SyntaxKind.EqualsToken, "="),
    tt(SyntaxKind.OpenParenthesisToken, "("),
    tt(SyntaxKind.CloseParenthesisToken, ")"),
    tt(SyntaxKind.AmpersandAmpersandToken, "&&"),
    tt(SyntaxKind.PipePipeToken, "||"),
    tt(SyntaxKind.EqualsEqualsToken, "=="),
    tt(SyntaxKind.BangEqualsToken, "!="),
    tt(SyntaxKind.FalseKeyword, "false"),
    tt(SyntaxKind.TrueKeyword, "true"),
  ]
  for t in tokens:
    yield t

iterator getSeparators(): TestToken =
  const tokens = [
    tt(SyntaxKind.WhitespaceToken, " "),
    tt(SyntaxKind.WhitespaceToken, "  "),
    tt(SyntaxKind.WhitespaceToken, "\r"),
    tt(SyntaxKind.WhitespaceToken, "\n"),
    tt(SyntaxKind.WhitespaceToken, "\r\n"),
  ]
  for t in tokens:
    yield t

proc requiresSeparator(t1Kind: SyntaxKind, t2Kind: SyntaxKind): bool =
  let t1IsKeyword = ($t1Kind).endsWith("Keyword")
  let t2IsKeyword = ($t2Kind).endsWith("Keyword")

  if (t1IsKeyword or t1Kind == SyntaxKind.IdentifierToken) and
      (t2IsKeyword or t2Kind == SyntaxKind.IdentifierToken):
    return true

  if t1Kind == SyntaxKind.NumberToken and t2Kind == SyntaxKind.NumberToken:
    return true

  if (
    t1Kind == SyntaxKind.WhitespaceToken and
    t2Kind == SyntaxKind.WhitespaceToken
  ):
    return true

  if t1Kind in {SyntaxKind.EqualsToken, SyntaxKind.BangToken} and
      t2Kind in {SyntaxKind.EqualsToken, SyntaxKind.EqualsEqualsToken}:
    return true

  return false

iterator getTokenPairs(): tuple[t1: TestToken, t2: TestToken] =
  for t1 in getTokens():
    for t2 in getTokens():
      if not requiresSeparator(t1.kind, t2.kind):
        yield (t1, t2)

iterator getTokenPairsWithSeparator(): tuple[
  t1: TestToken,
  sep: TestToken,
  t2: TestToken
] =
  for t1 in getTokens():
    for t2 in getTokens():
      if requiresSeparator(t1.kind, t2.kind):
        for sep in getSeparators():
          yield (t1, sep, t2)

suite "lexer":
  proc testLexerLexesToken(kind: SyntaxKind, text: string) =
    test fmt"lexer lexes token ({kind}, ""{text.readableEscape}"")":
      let token = parseTokens(text).assertSingle()
      assert token.kind == kind
      assert token.text == text

  proc testLexerLexesTokenPair(
    t1Kind: SyntaxKind,
    t1Text: string,
    t2Kind: SyntaxKind,
    t2Text: string
  ) =
    let text = t1Text & t2Text
    test (
      fmt"lexer lexes token pair ({t1Kind}, {t2Kind}," &
      fmt" ""{text.readableEscape}"")"
    ):
      let tokens = parseTokens(text)
      assert tokens.len == 2
      let t1 = tokens[0]
      let t2 = tokens[1]
      assert t1.kind == t1Kind
      assert t1.text == t1Text
      assert t2.kind == t2Kind
      assert t2.text == t2Text

  proc testLexerLexesTokenPairWithSeparator(
    t1Kind: SyntaxKind,
    t1Text: string,
    separatorKind: SyntaxKind,
    separatorText: string,
    t2Kind: SyntaxKind,
    t2Text: string
  ) =
    let text = t1Text & separatorText & t2Text
    test (
      fmt"lexer lexes token pair with separator ({t1Kind}, {t2Kind}," &
      fmt" ""{text.readableEscape}"")"
    ):
      let tokens = parseTokens(text)
      assert tokens.len == 3
      let t1 = tokens[0]
      let sep = tokens[1]
      let t2 = tokens[2]
      assert t1.kind == t1Kind
      assert t1.text == t1Text
      assert sep.kind == separatorKind
      assert sep.text == separatorText
      assert t2.kind == t2Kind
      assert t2.text == t2Text

  for t in getTokens().toSeq & getSeparators().toSeq:
    testLexerLexesToken(t.kind, t.text)

  for (t1, t2) in getTokenPairs():
    testLexerLexesTokenPair(
      t1.kind,
      t1.text,
      t2.kind,
      t2.text,
    )

  for (t1, sep, t2) in getTokenPairsWithSeparator():
    testLexerLexesTokenPairWithSeparator(
      t1.kind,
      t1.text,
      sep.kind,
      sep.text,
      t2.kind,
      t2.text,
    )
