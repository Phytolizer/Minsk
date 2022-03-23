from abc import ABC, abstractmethod

from minsk.analysis.binding.kind import BoundNodeKind


class BoundNode(ABC):
    @abstractmethod
    @property
    def kind(self) -> BoundNodeKind:
        pass
