from collections.abc import Iterable
from dataclasses import dataclass

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class NameExpressionSyntax(ExpressionSyntax):
    identifier_token: SyntaxToken

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.NameExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.identifier_token
