package dev.phytolizer.minsk

class SyntaxToken(val kind: SyntaxKind, val position: Int, val text: String, val value: Any?) {
    override fun toString(): String {
        val result = StringBuilder()
        result.append("$kind '$text'")
        if (value != null) {
            result.append(" $value")
        }
        return result.toString()
    }
}
