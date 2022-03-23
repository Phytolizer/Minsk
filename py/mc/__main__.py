from minsk.analysis.syntax.parser import Parser

while True:
    try:
        line = input("> ")
    except EOFError:
        break

    expression = Parser(line).parse()
    expression.pretty_print()
