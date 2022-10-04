package dev.phytolizer.minsk.analysis.text

data class TextSpan(val start: Int, val length: Int) {
    val end
        get() = start + length
    val range: IntRange
        get() = start until end

    companion object {
        fun fromBounds(start: Int, end: Int): TextSpan {
            return TextSpan(start, end - start)
        }
    }

    override fun toString(): String {
        return "$start..$end"
    }
}
