from typing import Any, Optional

from minsk.analysis.syntax.kind import SyntaxKind


class SyntaxToken:
    def __init__(self, kind: SyntaxKind, position: int, text: str, value: Optional[Any]):
        self.kind = kind
        self.position = position
        self.text = text
        self.value = value

    def __str__(self):
        result = f"{self.kind.name} '{self.text}'"
        if self.value is not None:
            result += f" {self.value}"
        return result
