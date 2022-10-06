import minsk/codeAnalysis/syntax/[
  expressionSyntax,
  syntaxKind,
  syntaxNode,
  syntaxToken,
]

type
  NameExpressionSyntax* = ref object of ExpressionSyntax
    identifierToken*: SyntaxToken

proc newNameExpressionSyntax*(identifierToken: SyntaxToken): NameExpressionSyntax =
  new(result)
  result.identifierToken = identifierToken

method kind*(self: NameExpressionSyntax): SyntaxKind =
  SyntaxKind.NameExpression

method children*(self: NameExpressionSyntax): seq[SyntaxNode] =
  result.add(self.identifierToken)
