import minsk/lexer
import minsk/syntaxKind
from minsk/macros import loop

when isMainModule:
  loop:
    stdout.write "> "
    stdout.flushFile()
    var line = ""
    if not stdin.readLine(line):
      echo ""
      break

    var lexer = newLexer(line)
    loop:
      let token = lexer.nextToken()
      echo token
      if token.kind == SyntaxKind.EndOfFileToken:
        break
