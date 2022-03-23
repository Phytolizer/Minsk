from typing import Iterable

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


class BinaryExpressionSyntax(ExpressionSyntax):
    def __init__(
        self,
        left: ExpressionSyntax,
        operator_token: SyntaxToken,
        right: ExpressionSyntax,
    ):
        self.left = left
        self.operator_token = operator_token
        self.right = right

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.BinaryExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.left
        yield self.operator_token
        yield self.right
