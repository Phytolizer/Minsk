import ../expressionSyntax
import ../syntaxKind
import ../syntaxNode
import ../syntaxToken

type
  LiteralExpressionSyntax* = ref object of ExpressionSyntax
    literalToken*: SyntaxToken

proc newLiteralExpressionSyntax*(literalToken: SyntaxToken): LiteralExpressionSyntax =
  new(result)
  result.literalToken = literalToken

method kind*(self: LiteralExpressionSyntax): SyntaxKind =
  return SyntaxKind.LiteralExpression

method children*(self: LiteralExpressionSyntax): seq[SyntaxNode] =
  result.add(self.literalToken)
