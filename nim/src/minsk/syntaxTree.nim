import expressionSyntax
import syntaxToken

type
  SyntaxTree* = object
    diagnostics*: seq[string]
    root*: ExpressionSyntax
    endOfFileToken*: SyntaxToken

proc newSyntaxTree*(
  diagnostics: seq[string],
  root: ExpressionSyntax,
  endOfFileToken: SyntaxToken
): SyntaxTree =
  result.diagnostics = diagnostics
  result.root = root
  result.endOfFileToken = endOfFileToken
