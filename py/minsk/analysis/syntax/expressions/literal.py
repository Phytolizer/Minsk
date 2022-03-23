from dataclasses import dataclass
from typing import Any, Iterable, Optional

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


@dataclass(frozen=True)
class LiteralExpressionSyntax(ExpressionSyntax):
    literal_token: SyntaxToken
    _value: Any

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.LiteralExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.literal_token

    @property
    def value(self) -> Optional[Any]:
        return self._value
