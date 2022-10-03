import syntaxKind

type
  SyntaxNode* = ref object of RootObj

method kind*(n: SyntaxNode): SyntaxKind {.base.} =
  discard

method children*(n: SyntaxNode): seq[SyntaxNode] {.base.} =
  discard
