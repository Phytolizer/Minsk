package dev.phytolizer.minsk

class SyntaxToken(override val kind: SyntaxKind, val position: Int, val text: String, val value: Any?) : SyntaxNode() {
    override fun toString(): String {
        val result = StringBuilder()
        result.append("$kind '$text'")
        if (value != null) {
            result.append(" $value")
        }
        return result.toString()
    }

    override val children: List<SyntaxNode>
        get() = listOf()
}
