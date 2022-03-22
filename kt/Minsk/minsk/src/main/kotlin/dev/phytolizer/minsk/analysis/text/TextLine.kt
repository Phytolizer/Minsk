package dev.phytolizer.minsk.analysis.text

class TextLine(private val _text: SourceText, val start: Int, val length: Int, val lengthIncludingLineBreak: Int) {
    val end: Int
        get() = start + length
    val endIncludingLineBreak: Int
        get() = start + lengthIncludingLineBreak
    val span: TextSpan
        get() = TextSpan(start, length)
    val spanIncludingLineBreak: TextSpan
        get() = TextSpan(start, lengthIncludingLineBreak)
}
