import syntaxKind

proc binaryOperatorPrecedence*(kind: SyntaxKind): int =
  case kind
  of SyntaxKind.StarToken, SyntaxKind.SlashToken:
    4
  of SyntaxKind.PlusToken, SyntaxKind.MinusToken:
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
    5
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
