import colorize/colorize

import minsk/parser
import minsk/macros
import minsk/minskObject
import minsk/syntaxNode
import minsk/syntaxToken

proc prettyPrint(
  node: SyntaxNode,
  indent: string = "",
  isLast: bool = true
) =
  let marker = if isLast: "└── " else: "├── "
  stdout.write(indent & marker & $node.kind)
  if node of SyntaxToken and (node.SyntaxToken).value.kind != mokNull:
    stdout.write(" " & $node.SyntaxToken.value)
  stdout.writeLine("")
  let indent = if isLast: indent & "    " else: indent & "│   "

  for i, child in node.children.pairs:
    prettyPrint(child, indent, i == node.children.high)

when isMainModule:
  loop:
    stdout.write "> "
    stdout.flushFile()
    var line = ""
    if not stdin.readLine(line):
      echo ""
      break

    var parser = newParser(line)
    let expression = parser.parse()
    stdout.setColor(styleDim, fgWhite)
    expression.prettyPrint()
    stdout.resetColor()
    stdout.flushFile()
