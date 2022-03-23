from abc import ABC, abstractmethod
from typing import Iterable

from minsk.analysis.syntax.kind import SyntaxKind


class SyntaxNode(ABC):
    @property
    @abstractmethod
    def kind(self) -> SyntaxKind:
        pass

    @property
    @abstractmethod
    def children(self) -> Iterable["SyntaxNode"]:
        pass
