import minskpkg/codeAnalysis/syntax/[
  expressionSyntax,
  syntaxKind,
  syntaxNode,
  syntaxToken,
]

type
  ParenthesizedExpressionSyntax* = ref object of ExpressionSyntax
    openParenthesisToken*: SyntaxToken
    expression*: ExpressionSyntax
    closeParenthesisToken*: SyntaxToken

proc newParenthesizedExpressionSyntax*(
  openParenthesisToken: SyntaxToken,
  expression: ExpressionSyntax,
  closeParenthesisToken: SyntaxToken
): ParenthesizedExpressionSyntax =
  new(result)
  result.openParenthesisToken = openParenthesisToken
  result.expression = expression
  result.closeParenthesisToken = closeParenthesisToken

method kind*(self: ParenthesizedExpressionSyntax): SyntaxKind =
  return SyntaxKind.ParenthesizedExpression

method children*(self: ParenthesizedExpressionSyntax): seq[SyntaxNode] =
  result.add(self.openParenthesisToken)
  result.add(self.expression)
  result.add(self.closeParenthesisToken)
