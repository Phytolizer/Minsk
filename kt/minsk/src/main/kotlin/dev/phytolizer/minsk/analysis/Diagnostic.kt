package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.text.TextSpan

class Diagnostic(val span: TextSpan, val message: String) {
    override fun toString() = message
}
