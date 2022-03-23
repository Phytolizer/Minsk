from typing import Any, Iterable, Optional

from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.token import SyntaxToken


class LiteralExpressionSyntax(ExpressionSyntax):
    literal_token: SyntaxToken
    _value: Any

    def __init__(self, literal_token: SyntaxToken, value: Optional[Any] = None):
        if value is None:
            value = literal_token.value
        self.literal_token = literal_token
        self._value = value

    @property
    def kind(self) -> SyntaxKind:
        return SyntaxKind.LiteralExpression

    @property
    def children(self) -> Iterable[SyntaxNode]:
        yield self.literal_token

    @property
    def value(self) -> Optional[Any]:
        return self._value
