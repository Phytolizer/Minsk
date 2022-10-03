import syntaxKind

proc binaryOperatorPrecedence*(kind: SyntaxKind): int =
  case kind
  of SyntaxKind.StarToken, SyntaxKind.SlashToken:
    2
  of SyntaxKind.PlusToken, SyntaxKind.MinusToken:
    1
  else:
    0
