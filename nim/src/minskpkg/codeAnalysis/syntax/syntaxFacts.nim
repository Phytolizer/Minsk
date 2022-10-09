import std/options
import std/sequtils

import syntaxKind

proc binaryOperatorPrecedence*(kind: SyntaxKind): int =
  case kind
  of SyntaxKind.StarToken, SyntaxKind.SlashToken:
    5
  of SyntaxKind.PlusToken, SyntaxKind.MinusToken:
    4
  of SyntaxKind.EqualsEqualsToken, SyntaxKind.BangEqualsToken:
    3
  of SyntaxKind.AmpersandAmpersandToken:
    2
  of SyntaxKind.PipePipeToken:
    1
  else:
    0

proc unaryOperatorPrecedence*(kind: SyntaxKind): int =
  case kind
  of SyntaxKind.PlusToken,
      SyntaxKind.MinusToken,
      SyntaxKind.BangToken:
    6
  else:
    0

proc keywordKind*(text: string): SyntaxKind =
  case text
  of "true":
    SyntaxKind.TrueKeyword
  of "false":
    SyntaxKind.FalseKeyword
  else:
    SyntaxKind.IdentifierToken

proc text*(kind: SyntaxKind): Option[string] =
  case kind
  of SyntaxKind.PlusToken:
    "+".some
  of SyntaxKind.MinusToken:
    "-".some
  of SyntaxKind.StarToken:
    "*".some
  of SyntaxKind.SlashToken:
    "/".some
  of SyntaxKind.BangToken:
    "!".some
  of SyntaxKind.EqualsToken:
    "=".some
  of SyntaxKind.AmpersandAmpersandToken:
    "&&".some
  of SyntaxKind.PipePipeToken:
    "||".some
  of SyntaxKind.EqualsEqualsToken:
    "==".some
  of SyntaxKind.BangEqualsToken:
    "!=".some
  of SyntaxKind.OpenParenthesisToken:
    "(".some
  of SyntaxKind.CloseParenthesisToken:
    ")".some
  of SyntaxKind.FalseKeyword:
    "false".some
  of SyntaxKind.TrueKeyword:
    "true".some
  else:
    none(string)

proc binaryOperators*(): seq[SyntaxKind] =
  SyntaxKind.toSeq.filter(
    proc(kind: SyntaxKind): bool =
      kind.binaryOperatorPrecedence > 0
  )

proc unaryOperators*(): seq[SyntaxKind] =
  SyntaxKind.toSeq.filter(
    proc(kind: SyntaxKind): bool =
      kind.unaryOperatorPrecedence > 0
  )
