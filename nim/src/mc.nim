import noise
import noise/styler

import ansi/ansi
import ansi/colorize

import minsk/codeAnalysis/compilation
import minsk/codeAnalysis/diagnostic
import minsk/codeAnalysis/evaluationResult
import minsk/macros
import minsk/minskObject
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
    var compilation = newCompilation(syntaxTree)
    let evaluationResult = compilation.evaluate()
    if evaluationResult.success:
      stdout.setColor(colorize.fgGreen)
      echo evaluationResult.value
      stdout.resetColor()
    else:
      stdout.setColor(colorize.fgRed)
      for diagnostic in evaluationResult.diagnostics:
        echo diagnostic
      stdout.resetColor()
    stdout.flushFile()

when isMainModule:
  main()
