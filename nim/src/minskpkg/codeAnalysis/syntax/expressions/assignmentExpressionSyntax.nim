import minskpkg/codeAnalysis/syntax/[
  expressionSyntax,
  syntaxKind,
  syntaxNode,
  syntaxToken,
]

type
  AssignmentExpressionSyntax* = ref object of ExpressionSyntax
    identifierToken*: SyntaxToken
    equalsToken*: SyntaxToken
    expression*: ExpressionSyntax

proc newAssignmentExpressionSyntax*(
  identifierToken,
  equalsToken: SyntaxToken,
  expression: ExpressionSyntax
): AssignmentExpressionSyntax =
  new(result)
  result.identifierToken = identifierToken
  result.equalsToken = equalsToken
  result.expression = expression

method kind*(self: AssignmentExpressionSyntax): SyntaxKind =
  SyntaxKind.AssignmentExpression

method children*(self: AssignmentExpressionSyntax): seq[SyntaxNode] =
  result.add(self.identifierToken)
  result.add(self.equalsToken)
  result.add(self.expression)
