import colorama
from colorama import Fore, Style
from colorama.ansi import Cursor, clear_screen
from minsk.analysis.binding.binder import Binder
from minsk.analysis.compilation import Compilation
from minsk.analysis.evaluator import Evaluator
from minsk.analysis.syntax.parser import Parser, SyntaxTree

colorama.init()

show_tree = False

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
    diagnostics, value = Compilation(syntax_tree).evaluate()
    if len(diagnostics) == 0:
        if show_tree:
            print(Fore.WHITE + Style.DIM, end="")
            syntax_tree.root.pretty_print()
            print(Style.RESET_ALL, end="")
        print(value)
    else:
        for diagnostic in diagnostics:
            prefix = line[: diagnostic.span.start]
            error = line[diagnostic.span.start : diagnostic.span.end]
            suffix = line[diagnostic.span.end :]

            print(Fore.RED, end="")
            print(diagnostic)
            print(Style.RESET_ALL, end="")
            print("    ", end="")
            print(prefix, end="")
            print(Fore.RED, end="")
            print(error, end="")
            print(Style.RESET_ALL, end="")
            print(suffix)
        print(Style.RESET_ALL, end="")
