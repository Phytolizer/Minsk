from collections.abc import Iterable
from dataclasses import dataclass

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken
from minsk.runtime.value import Value


@dataclass(frozen=True)
class LiteralExpressionSyntax(ExpressionSyntax):
    literal_token: SyntaxToken
    _value: Value

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.LiteralExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.literal_token

    @property
    def value(self) -> Value:
        return self._value
