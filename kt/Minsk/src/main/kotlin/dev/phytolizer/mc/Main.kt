package dev.phytolizer.mc

import dev.phytolizer.colors.AnsiColor
import dev.phytolizer.colors.ColorStyle
import dev.phytolizer.colors.Colorize
import dev.phytolizer.minsk.Parser

fun main() {
    while (true) {
        print("> ")
        val line = readLine() ?: break

        val parser = Parser(line)
        val expression = parser.parse()
        val diagnostics = parser.diagnostics

        if (diagnostics.isEmpty()) {
            expression.prettyPrint()
        } else {
            print(Colorize.colorCode(AnsiColor.Red, ColorStyle.Regular))
            for (diagnostic in diagnostics) {
                println(diagnostic)
            }
            print(Colorize.RESET)
        }
    }
}
