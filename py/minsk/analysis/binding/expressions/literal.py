from dataclasses import dataclass
from typing import Any, Type

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind


@dataclass(frozen=True)
class BoundLiteralExpression(BoundExpression):
    value: Any

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.LiteralExpression

    @property
    def ty(self) -> Type:
        return type(self.value)
