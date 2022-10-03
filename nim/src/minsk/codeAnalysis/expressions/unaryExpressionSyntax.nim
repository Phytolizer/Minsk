import ../expressionSyntax
import ../syntaxKind
import ../syntaxNode
import ../syntaxToken

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
