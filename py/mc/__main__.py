from typing import Any

import colorama
from colorama import Fore, Style
from colorama.ansi import Cursor, clear_screen

from minsk.analysis.compilation import Compilation
from minsk.analysis.syntax.parser import SyntaxTree
from minsk.analysis.variable import VariableSymbol

colorama.init()

show_tree = False
variables: dict[VariableSymbol, Any] = {}

while True:
    try:
        line = input("> ")
    except EOFError:
        break

    match line:
        case "#showTree":
            show_tree = not show_tree
            if show_tree:
                print("Showing parse trees.")
            else:
                print("Not showing parse trees.")
            continue
        case "#cls":
            print(clear_screen() + Cursor.POS(0, 0), end="")
            continue

    syntax_tree = SyntaxTree.parse(line)
    diagnostics, value = Compilation(syntax_tree).evaluate(variables)
    if len(diagnostics) == 0:
        if show_tree:
            print(Fore.WHITE + Style.DIM, end="")
            syntax_tree.root.pretty_print()
            print(Style.RESET_ALL, end="")
        print(value)
    else:
        for diagnostic in diagnostics:
            line_index = syntax_tree.text.get_line_index(diagnostic.span.start)
            line_number = line_index + 1
            character = (
                diagnostic.span.start - syntax_tree.text.lines[line_index].start + 1
            )

            prefix = line[: diagnostic.span.start]
            error = line[diagnostic.span.start : diagnostic.span.end]
            suffix = line[diagnostic.span.end :]

            print(Fore.RED, end="")
            print(f"({line_number}, {character}): ", end="")
            print(diagnostic)
            print(Style.RESET_ALL, end="")
            print("    ", end="")
            print(prefix, end="")
            print(Fore.RED, end="")
            print(error, end="")
            print(Style.RESET_ALL, end="")
            print(suffix)
        print(Style.RESET_ALL, end="")
