from abc import ABC, abstractmethod

from minsk.analysis.binding.kind import BoundNodeKind


class BoundNode(ABC):
    @property
    @abstractmethod
    def kind(self) -> BoundNodeKind:
        pass
