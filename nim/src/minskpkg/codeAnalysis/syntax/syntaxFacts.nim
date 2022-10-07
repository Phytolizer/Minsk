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
