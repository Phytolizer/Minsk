import noise
import noise/styler

import minskpkg/ansi/ansi
import minskpkg/ansi/colorize

import minskpkg/codeAnalysis/compilation
import minskpkg/codeAnalysis/diagnostic
import minskpkg/codeAnalysis/evaluationResult
import minskpkg/codeAnalysis/textSpan
import minskpkg/codeAnalysis/variableMap
import minskpkg/macros
import minskpkg/minskObject
import minskpkg/codeAnalysis/syntax/[
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
  var variables = newVariableMap()
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
    let evaluationResult = compilation.evaluate(variables)
    if evaluationResult.success:
      stdout.setColor(colorize.fgGreen)
      echo evaluationResult.value
      stdout.resetColor()
    else:
      for diagnostic in evaluationResult.diagnostics:
        stdout.setColor(colorize.fgRed)
        echo diagnostic
        stdout.resetColor()

        let prefix = line[0..<diagnostic.span.start]
        let error = line[diagnostic.span.start..<diagnostic.span.stop]
        let suffix = line[diagnostic.span.stop..<line.len]

        stdout.write "    " & prefix
        stdout.setColor(colorize.fgRed)
        stdout.write error
        stdout.resetColor()
        stdout.writeLine suffix
    stdout.flushFile()

when isMainModule:
  main()
