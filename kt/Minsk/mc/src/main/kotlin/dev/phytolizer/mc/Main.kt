package dev.phytolizer.mc

import dev.phytolizer.colors.AnsiColor
import dev.phytolizer.colors.ColorStyle
import dev.phytolizer.colors.Colorize
import dev.phytolizer.minsk.analysis.Compilation
import dev.phytolizer.minsk.analysis.VariableSymbol
import dev.phytolizer.minsk.analysis.syntax.SyntaxTree
import dev.phytolizer.minsk.analysis.text.TextSpan

fun main() {
    var showTree = false
    val variables = mutableMapOf<VariableSymbol, Any>()
    val textBuilder = StringBuilder()

    while (true) {
        if (textBuilder.isEmpty()) {
            print("> ")
        } else {
            print("| ")
        }
        val input = readLine() ?: break
        if (textBuilder.isEmpty()) {
            when (input) {
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
        }

        textBuilder.appendLine(input)
        val text = textBuilder.toString()

        val syntaxTree = SyntaxTree.parse(text)

        val isBlank = input.all { it.isWhitespace() }
        if (!isBlank && syntaxTree.diagnostics.isNotEmpty()) {
            continue
        }

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
            for (diagnostic in diagnostics) {
                val lineIndex = syntaxTree.text.lineIndex(diagnostic.span.start)
                val line = syntaxTree.text.lines[lineIndex]
                val lineNumber = lineIndex + 1
                val character = diagnostic.span.start - line.start + 1

                val prefixSpan = TextSpan.fromBounds(line.start, diagnostic.span.start)
                val suffixSpan = TextSpan.fromBounds(diagnostic.span.end, line.end)

                val prefix = syntaxTree.text.toString(prefixSpan)
                val error = syntaxTree.text.toString(diagnostic.span)
                val suffix = syntaxTree.text.toString(suffixSpan)

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

        textBuilder.clear()
    }
}
