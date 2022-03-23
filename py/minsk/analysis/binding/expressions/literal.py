from typing import Any, Type

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind


class BoundLiteralExpression(BoundExpression):
    value: Any

    def __init__(self, value: Any):
        self.value = value

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.LiteralExpression

    @property
    def ty(self) -> Type:
        return type(self.value)
