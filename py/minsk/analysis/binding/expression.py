from abc import ABC, abstractmethod
from typing import Type

from minsk.analysis.binding.node import BoundNode


class BoundExpression(BoundNode, ABC):
    @abstractmethod
    @property
    def ty(self) -> Type:
        pass
