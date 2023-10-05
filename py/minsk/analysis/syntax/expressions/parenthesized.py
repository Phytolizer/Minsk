from collections.abc import Iterable
from dataclasses import dataclass

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class ParenthesizedExpressionSyntax(ExpressionSyntax):
    open_parenthesis_token: SyntaxToken
    expression: ExpressionSyntax
    close_parenthesis_token: SyntaxToken

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.ParenthesizedExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.open_parenthesis_token
        yield self.expression
        yield self.close_parenthesis_token
