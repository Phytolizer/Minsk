from collections.abc import Iterable
from dataclasses import dataclass

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class BinaryExpressionSyntax(ExpressionSyntax):
    left: ExpressionSyntax
    operator_token: SyntaxToken
    right: ExpressionSyntax

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.BinaryExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.left
        yield self.operator_token
        yield self.right
