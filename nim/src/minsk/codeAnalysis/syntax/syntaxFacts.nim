import syntaxKind

proc binaryOperatorPrecedence*(kind: SyntaxKind): int =
  case kind
  of SyntaxKind.StarToken, SyntaxKind.SlashToken:
    2
  of SyntaxKind.PlusToken, SyntaxKind.MinusToken:
    1
  else:
    0

proc unaryOperatorPrecedence*(kind: SyntaxKind): int =
  case kind
  of SyntaxKind.PlusToken, SyntaxKind.MinusToken:
    3
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
