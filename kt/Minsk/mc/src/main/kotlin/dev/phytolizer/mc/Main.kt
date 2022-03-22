package dev.phytolizer.mc

import dev.phytolizer.colors.AnsiColor
import dev.phytolizer.colors.ColorStyle
import dev.phytolizer.colors.Colorize
import dev.phytolizer.minsk.analysis.Compilation
import dev.phytolizer.minsk.analysis.VariableSymbol
import dev.phytolizer.minsk.analysis.syntax.SyntaxTree

fun main() {
    var showTree = false
    val variables = mutableMapOf<VariableSymbol, Any>()

    while (true) {
        print("> ")
        val line = readLine() ?: break
        when (line) {
            "#showTree" -> {
                showTree = !showTree
                if (showTree) {
                    println("Showing parse trees.")
                } else {
                    println("Not showing parse trees.")
                }
                continue
            }
            "#cls" -> {
                Colorize.clearScreen()
                continue
            }
        }

        val syntaxTree = SyntaxTree.parse(line)
        val result = Compilation().evaluate(syntaxTree, variables)
        val diagnostics = result.diagnostics

        if (diagnostics.isEmpty()) {
            if (showTree) {
                print(Colorize.colorCode256(243))
                syntaxTree.root.prettyPrint()
                print(Colorize.RESET)
            }

            println(result.value)
        } else {
            val text = syntaxTree.text

            for (diagnostic in diagnostics) {
                val lineIndex = text.lineIndex(diagnostic.span.start)
                val lineNumber = lineIndex + 1
                val character = diagnostic.span.start - text.lines[lineIndex].start + 1

                val prefix = line.substring(0 until diagnostic.span.start)
                val error = line.substring(diagnostic.span.range)
                val suffix = line.substring(diagnostic.span.end until line.length)

                print(Colorize.colorCode(AnsiColor.Red, ColorStyle.Regular))
                print("($lineNumber, $character): ")
                println(diagnostic)
                print(Colorize.RESET)
                println()
                print("    ")
                print(prefix)
                print(Colorize.colorCode(AnsiColor.Red, ColorStyle.Regular))
                print(error)
                print(Colorize.RESET)
                println(suffix)
                println()
            }
            print(Colorize.RESET)
        }
    }
}
