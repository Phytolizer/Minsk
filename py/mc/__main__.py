from minsk.analysis.evaluator import Evaluator
from minsk.analysis.syntax.parser import Parser

while True:
    try:
        line = input("> ")
    except EOFError:
        break

    parser = Parser(line)
    expression = parser.parse()
    diagnostics = parser.diagnostics
    if len(diagnostics) == 0:
        expression.pretty_print()
        evaluator = Evaluator(expression)
        print(evaluator.evaluate())
    else:
        for diagnostic in diagnostics:
            print(diagnostic)
