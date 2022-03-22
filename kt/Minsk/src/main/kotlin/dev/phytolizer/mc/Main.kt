package dev.phytolizer.mc

import dev.phytolizer.minsk.Lexer

fun main() {
    while (true) {
        print("> ")
        val line = readLine() ?: break

        for (token in Lexer(line)) {
            println(token)
        }
    }
}
