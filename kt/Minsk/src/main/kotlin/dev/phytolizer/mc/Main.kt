package dev.phytolizer.mc

import dev.phytolizer.minsk.Parser

fun main() {
    while (true) {
        print("> ")
        val line = readLine() ?: break

        val parser = Parser(line)
        val expression = parser.parse()

        expression.prettyPrint()
    }
}
