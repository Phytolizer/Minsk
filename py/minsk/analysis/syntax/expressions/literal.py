from typing import Iterable

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


class LiteralExpressionSyntax(ExpressionSyntax):
    def __init__(self, literal_token: SyntaxToken):
        self.literal_token = literal_token

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.LiteralExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.literal_token
