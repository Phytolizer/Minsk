import colorama
from colorama import Fore, Style
from colorama.ansi import Cursor, clear_screen
from minsk.analysis.evaluator import Evaluator
from minsk.analysis.syntax.parser import Parser

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

    parser = Parser(line)
    expression = parser.parse()
    diagnostics = parser.diagnostics
    if len(diagnostics) == 0:
        print(Fore.WHITE + Style.DIM, end="")
        expression.root.pretty_print()
        print(Style.RESET_ALL, end="")
        evaluator = Evaluator(expression.root)
        print(evaluator.evaluate())
    else:
        print(Fore.RED, end="")
        for diagnostic in diagnostics:
            print(diagnostic)
        print(Style.RESET_ALL, end="")
