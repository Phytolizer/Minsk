package dev.phytolizer.mc

fun main() {
    while (true) {
        val line = readLine() ?: break

        if (line == "1 + 2 * 3") {
            println("7")
        } else {
            println("ERROR: Invalid expression!")
        }
    }
}
