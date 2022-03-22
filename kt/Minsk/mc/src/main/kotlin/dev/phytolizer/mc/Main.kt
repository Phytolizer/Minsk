package dev.phytolizer.mc

import dev.phytolizer.colors.AnsiColor
import dev.phytolizer.colors.ColorStyle
import dev.phytolizer.colors.Colorize
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
        val result = syntaxTree.evaluate()
        val diagnostics = listOf(syntaxTree.diagnostics, result.diagnostics).flatten()

        if (diagnostics.isEmpty()) {
            if (showTree) {
                print(Colorize.colorCode256(243))
                syntaxTree.root.prettyPrint()
                print(Colorize.RESET)
            }

            println(result.value)
        } else {
            print(Colorize.colorCode(AnsiColor.Red, ColorStyle.Regular))
            for (diagnostic in diagnostics) {
                println(diagnostic)
            }
            print(Colorize.RESET)
        }
    }
}
