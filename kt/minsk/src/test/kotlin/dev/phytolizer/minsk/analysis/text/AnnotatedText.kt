package dev.phytolizer.minsk.analysis.text

class AnnotatedText private constructor(val text: String, val spans: List<TextSpan>) {
    companion object {
        fun parse(text: String): AnnotatedText {
            val unindentedText = unindent(text)
            val textBuilder = StringBuilder()
            val spanBuilder = mutableListOf<TextSpan>()
            val startStack = mutableListOf<Int>()
            var position = 0
            for (c in unindentedText) {
                when (c) {
                    '[' -> {
                        startStack.add(position)
                    }
                    ']' -> {
                        if (startStack.size == 0) {
                            throw Exception("Too many ']' in text")
                        }
                        val start = startStack.removeLast()
                        val end = position
                        val span = TextSpan.fromBounds(start, end)
                        spanBuilder.add(span)
                    }
                    else -> {
                        position += 1
                        textBuilder.append(c)
                    }
                }
            }

            if (startStack.isNotEmpty()) {
                throw IllegalArgumentException("Too many '[' in text")
            }
            return AnnotatedText(textBuilder.toString(), spanBuilder)
        }

        fun unindentLines(text: String): List<String> {
            val lines = text.split("\r\n", "\n")
            val minIndentation = lines.filter { it.isNotBlank() }.minOf { it.length - it.trimStart().length }
            val dedentedLines = lines.map {
                if (it.isNotBlank()) {
                    it.substring(minIndentation)
                } else {
                    it
                }
            }
            val skipBegin = dedentedLines.takeWhile { it.isBlank() }.count()
            val skipEnd = dedentedLines.takeLastWhile { it.isBlank() }.count()

            return dedentedLines.subList(skipBegin, dedentedLines.size - skipEnd)
        }

        private fun unindent(text: String): String {
            return unindentLines(text).joinToString("\n")
        }
    }
}
