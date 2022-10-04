import noise
import noise/styler

import ansi/ansi
import ansi/colorize

import minsk/codeAnalysis/evaluator
import minsk/macros
import minsk/minskObject
import minsk/codeAnalysis/binding/binder
import minsk/codeAnalysis/syntax/[
  parser,
  syntaxNode,
  syntaxToken,
]

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

proc main() =
  var showTree = false
  var n = Noise.init()
  discard n.historyLoad("minsk.history")
  defer:
    discard n.historySave("minsk.history")
  loop:
    n.setPrompt("> ")
    if not n.readLine():
      break
    let line = n.getLine()
    n.historyAdd(line)

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
      stdout.setColor(colorize.styleDim, colorize.fgWhite)
      prettyPrint(syntaxTree.root)
      stdout.resetColor()
      stdout.flushFile()
    var binder = newBinder()
    let boundRoot = binder.bindExpression(syntaxTree.root)
    let diagnostics = syntaxTree.diagnostics & binder.diagnostics
    if diagnostics.len > 0:
      stdout.setColor(colorize.fgRed)
      for diagnostic in diagnostics:
        echo diagnostic
      stdout.resetColor()
    else:
      let evaluator = newEvaluator(boundRoot)
      let result = evaluator.evaluate()
      stdout.setColor(colorize.fgGreen)
      echo result
      stdout.resetColor()
    stdout.flushFile()

when isMainModule:
  main()
