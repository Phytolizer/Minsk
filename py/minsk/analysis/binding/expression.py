from abc import ABC, abstractmethod

from minsk.analysis.binding.node import BoundNode
from minsk.analysis.type import MinskType


class BoundExpression(BoundNode, ABC):
    @property
    @abstractmethod
    def ty(self) -> MinskType:
        pass
