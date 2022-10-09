import std/options
import std/strformat
import std/unittest

import minskpkg/codeAnalysis/syntax/[
  parser,
  syntaxFacts,
  syntaxKind,
  syntaxToken,
]

import helpers

suite "syntax facts":
  proc getTextRoundTrip(kind: SyntaxKind) =
    test fmt"text() round trip ({kind})":
      let text = kind.text
      if text.isNone:
        return
      let token = parseTokens(text.get).assertSingle()
      assert token.kind == kind
      assert token.text == text.get

  for kind in SyntaxKind:
    getTextRoundTrip(kind)
