package dev.phytolizer.mc

import dev.phytolizer.colors.AnsiColor
import dev.phytolizer.colors.ColorStyle
import dev.phytolizer.colors.Colorize
import dev.phytolizer.minsk.analysis.Compilation
import dev.phytolizer.minsk.analysis.syntax.SyntaxTree

fun main() {
    var showTree = false

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
        val result = Compilation().evaluate(syntaxTree)
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
                val prefix = line.substring(0 until diagnostic.span.start)
                val error = line.substring(diagnostic.span.range)
                val suffix = line.substring(diagnostic.span.end until line.length)

                print(Colorize.colorCode(AnsiColor.Red, ColorStyle.Regular))
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
