import colorama
from colorama import Fore, Style

from minsk.analysis.evaluator import Evaluator
from minsk.analysis.syntax.parser import Parser

colorama.init()

while True:
    try:
        line = input("> ")
    except EOFError:
        break

    parser = Parser(line)
    expression = parser.parse()
    diagnostics = parser.diagnostics
    if len(diagnostics) == 0:
        print(Fore.WHITE + Style.DIM, end="")
        expression.pretty_print()
        print(Style.RESET_ALL, end="")
        evaluator = Evaluator(expression)
        print(evaluator.evaluate())
    else:
        print(Fore.RED, end="")
        for diagnostic in diagnostics:
            print(diagnostic)
        print(Style.RESET_ALL, end="")
