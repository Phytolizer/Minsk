import sys
from abc import ABC, abstractmethod
from collections.abc import Iterable
from enum import Enum, auto
from io import StringIO
from typing import TextIO

from colorama import Fore, Style

from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.text.span import TextSpan
from minsk.runtime.value import Value


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
    def value(self) -> Value:
        return None

    @property
    def span(self) -> TextSpan:
        children = tuple(self.children)
        first = children[0]
        last = children[-1]
        return TextSpan.from_bounds(first.span.start, last.span.end)

    class _OutputKind(Enum):
        Console = auto()
        Redirected = auto()

    class _OutputStyle(Enum):
        Regular = auto()
        Last = auto()

    def pretty_print(self) -> None:
        SyntaxNode._pretty_print(self, sys.stdout, self._OutputKind.Console)

    def write_to(self, writer: TextIO) -> None:
        SyntaxNode._pretty_print(self, writer, self._OutputKind.Redirected)

    def __str__(self) -> str:
        writer = StringIO()
        self.write_to(writer)
        return str(writer)

    @staticmethod
    def _pretty_print(
        node: "SyntaxNode",
        writer: TextIO,
        output: _OutputKind,
        indent: str = "",
        style: _OutputStyle = _OutputStyle.Last,
    ) -> None:
        if output == SyntaxNode._OutputKind.Console:
            writer.write(Fore.WHITE + Style.DIM)
        writer.write(indent)
        marker = "└── " if style == SyntaxNode._OutputStyle.Last else "├── "
        writer.write(marker)
        if output == SyntaxNode._OutputKind.Console:
            writer.write(Style.NORMAL + (Fore.CYAN if node.is_token() else Fore.BLUE))
        writer.write(node.kind.name)
        if output == SyntaxNode._OutputKind.Console:
            writer.write(Style.RESET_ALL)
        if node.is_token() and node.value is not None:
            writer.write(f" {node.value}")
        writer.write("\n")
        if style == SyntaxNode._OutputStyle.Last:
            indent += "    "
        else:
            indent += "│   "
        children = tuple(node.children)
        if len(children) > 0:
            last_child = children[-1]
            for child in children:
                SyntaxNode._pretty_print(
                    child,
                    writer,
                    output,
                    indent,
                    SyntaxNode._OutputStyle.Last
                    if child is last_child
                    else SyntaxNode._OutputStyle.Regular,
                )
