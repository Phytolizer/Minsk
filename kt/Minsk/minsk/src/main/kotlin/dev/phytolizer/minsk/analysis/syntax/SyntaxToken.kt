package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.TextSpan

class SyntaxToken(override val kind: SyntaxKind, val position: Int, val text: String, val value: Any?) : SyntaxNode() {
    override fun toString(): String {
        val result = StringBuilder()
        result.append("$kind '$text'")
        if (value != null) {
            result.append(" $value")
        }
        return result.toString()
    }

    override val span: TextSpan
        get() = TextSpan(position, text.length)
    override val children: List<SyntaxNode>
        get() = listOf()
}
