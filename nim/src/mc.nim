import minsk/lexer
import minsk/syntaxKind

when isMainModule:
  while true:
    stdout.write "> "
    stdout.flushFile()
    var line = ""
    if not stdin.readLine(line):
      echo ""
      break

    var lexer = newLexer(line)
    while true:
      let token = lexer.nextToken()
      echo token
      if token.kind == SyntaxKind.EndOfFileToken:
        break
