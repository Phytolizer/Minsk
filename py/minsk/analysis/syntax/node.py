from abc import ABC, abstractmethod
from typing import Any, Iterable, Optional

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

    @staticmethod
    def is_token() -> bool:
        return False

    @property
    def value(self) -> Optional[Any]:
        return None

    def pretty_print(self):
        SyntaxNode._pretty_print(self, "", True)

    @staticmethod
    def _pretty_print(node: "SyntaxNode", indent: str, is_last: bool):
        print(indent, end="")
        if is_last:
            marker = "└── "
        else:
            marker = "├── "
        print(marker, end="")
        print(node.kind.name, end="")
        if node.is_token() and node.value is not None:
            print(f" {node.value}", end="")
        print()
        if is_last:
            indent += "    "
        else:
            indent += "│   "
        children = tuple(node.children)
        if len(children) > 0:
            last_child = children[-1]
            for child in children:
                SyntaxNode._pretty_print(child, indent, child is last_child)
