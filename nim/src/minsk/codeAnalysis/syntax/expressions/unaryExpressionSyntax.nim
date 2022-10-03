import minsk/codeAnalysis/syntax/[
  expressionSyntax,
  syntaxKind,
  syntaxNode,
  syntaxToken,
]

type
  UnaryExpressionSyntax* = ref object of ExpressionSyntax
    operatorToken*: SyntaxToken
    operand*: ExpressionSyntax

proc newUnaryExpressionSyntax*(
  operatorToken: SyntaxToken,
  operand: ExpressionSyntax
): UnaryExpressionSyntax =
  new(result)
  result.operatorToken = operatorToken
  result.operand = operand

method kind*(self: UnaryExpressionSyntax): SyntaxKind =
  return SyntaxKind.UnaryExpression

method children*(self: UnaryExpressionSyntax): seq[SyntaxNode] =
  result.add(self.operatorToken)
  result.add(self.operand)
