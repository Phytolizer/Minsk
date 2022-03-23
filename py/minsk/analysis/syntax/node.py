import sys
from abc import ABC, abstractmethod
from io import StringIO
from typing import Any, Iterable, Optional, TextIO

from colorama import Fore, Style

from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.text.span import TextSpan


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

    @property
    def span(self) -> TextSpan:
        children = tuple(self.children)
        first = children[0]
        last = children[-1]
        return TextSpan.from_bounds(first.span.start, last.span.end)

    def pretty_print(self):
        SyntaxNode._pretty_print(self, sys.stdout, True, "", True)

    def write_to(self, writer: TextIO):
        SyntaxNode._pretty_print(self, writer, False, "", True)

    def __str__(self):
        writer = StringIO()
        self.write_to(writer)
        return str(writer)

    @staticmethod
    def _pretty_print(
        node: "SyntaxNode",
        writer: TextIO,
        is_to_console: bool,
        indent: str,
        is_last: bool,
    ):
        if is_to_console:
            writer.write(Fore.WHITE + Style.DIM)
        writer.write(indent)
        if is_last:
            marker = "└── "
        else:
            marker = "├── "
        writer.write(marker)
        if is_to_console:
            writer.write(Style.NORMAL + (Fore.CYAN if node.is_token() else Fore.BLUE))
        writer.write(node.kind.name)
        if is_to_console:
            writer.write(Style.RESET_ALL)
        if node.is_token() and node.value is not None:
            writer.write(f" {node.value}")
        writer.write("\n")
        if is_last:
            indent += "    "
        else:
            indent += "│   "
        children = tuple(node.children)
        if len(children) > 0:
            last_child = children[-1]
            for child in children:
                SyntaxNode._pretty_print(
                    child, writer, is_to_console, indent, child is last_child
                )
