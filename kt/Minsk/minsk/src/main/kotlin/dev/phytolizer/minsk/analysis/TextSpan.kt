package dev.phytolizer.minsk.analysis

class TextSpan(val start: Int, val length: Int) {
    val end
        get() = start + length
    val range: IntRange
        get() = start until end
}
