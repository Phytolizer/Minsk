import ansi/ansi
import ansi/colorize

import minsk/codeAnalysis/evaluator
import minsk/codeAnalysis/macros
import minsk/codeAnalysis/minskObject
import minsk/codeAnalysis/parser
import minsk/codeAnalysis/syntaxNode
import minsk/codeAnalysis/syntaxToken

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
  var showTree = false
  loop:
    stdout.write "> "
    stdout.flushFile()
    var line = ""
    if not stdin.readLine(line):
      echo ""
      break

    if line == "#showTree":
      showTree = not showTree
      echo if showTree: "Showing parse trees." else: "Not showing parse trees."
      continue
    if line == "#cls":
      stdout.clear()
      stdout.flushFile()
      continue

    let syntaxTree = parse(line)
    if showTree:
      stdout.setColor(styleDim, fgWhite)
      prettyPrint(syntaxTree.root)
      stdout.resetColor()
      stdout.flushFile()
    let diagnostics = syntaxTree.diagnostics
    if diagnostics.len > 0:
      stdout.setColor(fgRed)
      for diagnostic in diagnostics:
        echo diagnostic
      stdout.resetColor()
    else:
      let evaluator = newEvaluator(syntaxTree.root)
      let result = evaluator.evaluate()
      stdout.setColor(fgGreen)
      echo result
      stdout.resetColor()
    stdout.flushFile()
