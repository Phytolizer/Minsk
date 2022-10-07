import minskpkg/minskObject
import minskpkg/codeAnalysis/syntax/[
  expressionSyntax,
  syntaxKind,
  syntaxNode,
  syntaxToken,
]

type
  LiteralExpressionSyntax* = ref object of ExpressionSyntax
    literalToken*: SyntaxToken
    value*: MinskObject

proc newLiteralExpressionSyntax*(
  literalToken: SyntaxToken,
  value: MinskObject = literalToken.value
): LiteralExpressionSyntax =
  new(result)
  result.literalToken = literalToken
  result.value = value

method kind*(self: LiteralExpressionSyntax): SyntaxKind =
  return SyntaxKind.LiteralExpression

method children*(self: LiteralExpressionSyntax): seq[SyntaxNode] =
  result.add(self.literalToken)
