package dev.phytolizer.minsk.analysis.text

class SourceText private constructor(private val _text: String) {
    val lines = parseLines(this, _text)
    val length = _text.length

    companion object {
        fun from(text: String): SourceText {
            return SourceText(text)
        }

        private fun parseLines(sourceText: SourceText, text: String): List<TextLine> {
            val result = mutableListOf<TextLine>()

            var position = 0
            var lineStart = 0

            while (position < text.length) {
                val lineBreakWidth = getLineBreakWidth(text, position)

                if (lineBreakWidth == 0) {
                    position += 1
                } else {
                    addLine(result, sourceText, position, lineStart, lineBreakWidth)

                    position += lineBreakWidth
                    lineStart = position
                }
            }

            addLine(result, sourceText, position, lineStart, 0)

            return result
        }

        private fun addLine(
            lines: MutableList<TextLine>,
            sourceText: SourceText,
            position: Int,
            lineStart: Int,
            lineBreakWidth: Int,
        ) {
            val lineLength = position - lineStart
            val lineLengthIncludingLineBreak = lineLength + lineBreakWidth
            val line = TextLine(sourceText, lineStart, lineLength, lineLengthIncludingLineBreak)
            lines.add(line)
        }

        private fun getLineBreakWidth(text: String, position: Int): Int {
            val char = text[position]
            val look = if (position + 1 >= text.length) {
                '\u0000'
            } else {
                text[position + 1]
            }

            return if (char == '\r' && look == '\n') {
                2
            } else if (char == '\r' || char == '\n') {
                1
            } else {
                0
            }
        }
    }

    fun lineIndex(position: Int): Int {
        var lower = 0
        var upper = lines.size - 1

        while (lower <= upper) {
            val index = lower + (upper - lower) / 2
            val start = lines[index].start

            if (position == start) {
                return index
            }

            if (start > position) {
                upper = index - 1
            } else {
                lower = index + 1
            }
        }

        return lower - 1
    }

    operator fun get(index: Int) = _text[index]

    override fun toString() = _text
    fun toString(range: IntRange) = _text.substring(range)
    fun toString(span: TextSpan) = _text.substring(span.range)
}
