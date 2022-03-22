package dev.phytolizer.mc

import dev.phytolizer.colors.AnsiColor
import dev.phytolizer.colors.ColorStyle
import dev.phytolizer.colors.Colorize
import dev.phytolizer.minsk.analysis.Evaluator
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
        val diagnostics = syntaxTree.diagnostics

        if (diagnostics.isEmpty()) {
            if (showTree) {
                print(Colorize.colorCode256(243))
                syntaxTree.root.prettyPrint()
                print(Colorize.RESET)
            }

            val result = Evaluator().evaluate(syntaxTree.root)
            println(result)
        } else {
            print(Colorize.colorCode(AnsiColor.Red, ColorStyle.Regular))
            for (diagnostic in diagnostics) {
                println(diagnostic)
            }
            print(Colorize.RESET)
        }
    }
}
