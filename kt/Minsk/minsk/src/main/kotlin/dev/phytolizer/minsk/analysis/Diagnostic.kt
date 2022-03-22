package dev.phytolizer.minsk.analysis

class Diagnostic(val span: TextSpan, val message: String) {
    override fun toString() = message
}
