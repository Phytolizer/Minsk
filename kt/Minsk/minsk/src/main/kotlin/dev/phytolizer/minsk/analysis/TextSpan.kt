package dev.phytolizer.minsk.analysis

class TextSpan(val start: Int, val length: Int) {
    val end
        get() = start + length
}
