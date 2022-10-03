import ../expressionSyntax
import ../syntaxKind
import ../syntaxNode
import ../syntaxToken

type
  BinaryExpressionSyntax* = ref object of ExpressionSyntax
    left*: ExpressionSyntax
    operatorToken*: SyntaxToken
    right*: ExpressionSyntax

proc newBinaryExpressionSyntax*(
  left: ExpressionSyntax,
  operatorToken: SyntaxToken,
  right: ExpressionSyntax
): BinaryExpressionSyntax =
  return BinaryExpressionSyntax(left: left, operatorToken: operatorToken, right: right)

method kind*(self: BinaryExpressionSyntax): SyntaxKind =
  return SyntaxKind.BinaryExpression

method children*(self: BinaryExpressionSyntax): seq[SyntaxNode] =
  result.add(self.left)
  result.add(self.operatorToken)
  result.add(self.right)
