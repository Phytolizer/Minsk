from typing import Any, Iterable, Optional

from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode


class SyntaxToken(SyntaxNode):
    def __init__(
            self, kind: SyntaxKind, position: int, text: str, value: Optional[Any]
    ):
        self._kind = kind
        self.position = position
        self.text = text
        self._value = value

    def __str__(self):
        result = f"{self.kind.name} '{self.text}'"
        if self.value is not None:
            result += f" {self.value}"
        return result

    @property
    def kind(self) -> SyntaxKind:
        return self._kind

    @property
    def children(self) -> Iterable[SyntaxNode]:
        return ()

    @property
    def value(self) -> Optional[Any]:
        return self._value
