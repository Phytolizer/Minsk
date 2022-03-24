package dev.phytolizer.minsk.analysis.text

class TextSpan(val start: Int, val length: Int) {
    val end
        get() = start + length
    val range: IntRange
        get() = start until end

    companion object {
        fun fromBounds(start: Int, end: Int): TextSpan {
            return TextSpan(start, end - start)
        }
    }

    override fun equals(other: Any?): Boolean {
        if (other !is TextSpan) {
            return false
        }
        return other.start == start && other.length == length
    }
}
