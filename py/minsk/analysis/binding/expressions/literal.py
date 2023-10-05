from dataclasses import dataclass

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.binding.kind import BoundNodeKind
from minsk.analysis.type import MinskType
from minsk.runtime.value import Value


@dataclass(frozen=True)
class BoundLiteralExpression(BoundExpression):
    value: Value

    @property
    def kind(self) -> BoundNodeKind:
        return BoundNodeKind.LiteralExpression

    @property
    def ty(self) -> MinskType:
        if isinstance(self.value, bool):
            return MinskType.Bool
        if isinstance(self.value, int):
            return MinskType.Int
        raise Exception(f"Unsupported variable type {type(self.value)}")
