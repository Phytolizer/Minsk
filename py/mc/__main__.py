from minsk.analysis.syntax.lexer import Lexer

while True:
    try:
        line = input("> ")
    except EOFError:
        break

    for token in Lexer(line):
        print(token)
