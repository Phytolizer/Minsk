import syntaxNode
import syntaxKind

type
  ExpressionSyntax* = ref object of SyntaxNode

method kind*(e: ExpressionSyntax): SyntaxKind =
  discard
