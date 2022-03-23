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
text_builder = ""

while True:
    if len(text_builder) == 0:
        print("> ", end="")
    else:
        print("| ", end="")
    try:
        input_line = input()
    except EOFError:
        break

    if len(text_builder) == 0:
        match input_line:
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

    is_blank = input_line == ""
    text_builder += input_line + "\n"

    syntax_tree = SyntaxTree.parse(text_builder)
    if (not is_blank) and len(syntax_tree.diagnostics) > 0:
        continue
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
            line = syntax_tree.text.lines[line_index]
            character = diagnostic.span.start - line.start + 1

            prefix = text_builder[line.start : diagnostic.span.start]
            error = text_builder[diagnostic.span.start : diagnostic.span.end]
            suffix = text_builder[diagnostic.span.end : line.end]

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

    text_builder = ""
