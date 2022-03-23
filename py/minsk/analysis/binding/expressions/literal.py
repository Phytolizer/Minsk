from dataclasses import dataclass
from typing import Any

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.type import MinskType


@dataclass(frozen=True)
class BoundLiteralExpression(BoundExpression):
    value: Any

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.LiteralExpression

    @property
    def ty(self) -> MinskType:
        if isinstance(self.value, int):
            return MinskType.Int
        if isinstance(self.value, bool):
            return MinskType.Bool
        raise Exception(f"Unsupported variable type {type(self.value)}")
