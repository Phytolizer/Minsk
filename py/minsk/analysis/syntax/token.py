from collections.abc import Iterable

from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.text.span import TextSpan
from minsk.runtime.value import Value


class SyntaxToken(SyntaxNode):
    def __init__(
        self, kind: SyntaxKind, position: int, text: str, value: Value
    ) -> None:
        self._kind = kind
        self.position = position
        self.text = text
        self._value = value

    def __str__(self) -> str:
        result = f"{self.kind.name} '{self.text}'"
        if self.value is not None:
            result += f" {self.value}"
        return result

    @property
    def kind(self) -> SyntaxKind:
        return self._kind

    @property
    def span(self) -> TextSpan:
        return TextSpan(self.position, len(self.text))

    @property
    def children(self) -> Iterable[SyntaxNode]:
        return ()

    @staticmethod
    def is_token() -> bool:
        return True

    @property
    def value(self) -> Value:
        return self._value
