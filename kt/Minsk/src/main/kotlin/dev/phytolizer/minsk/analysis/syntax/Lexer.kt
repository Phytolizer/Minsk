package dev.phytolizer.minsk.analysis.syntax

class Lexer(private val _text: String) : Iterable<SyntaxToken> {
    private val _diagnostics = mutableListOf<String>()
    val diagnostics: List<String>
        get() = _diagnostics

    override fun iterator(): Iterator<SyntaxToken> {
        return LexerIterator(this)
    }

    private class LexerIterator(private val lexer: Lexer) : Iterator<SyntaxToken> {
        private var position = 0
        private val current: Char
            get() {
                return if (position >= lexer._text.length) {
                    '\u0000'
                } else {
                    lexer._text[position]
                }
            }

        override fun hasNext(): Boolean {
            return position < lexer._text.length
        }

        override fun next(): SyntaxToken {
            val start = position
            var currentText: String? = null
            var value: Any? = null
            var kind = SyntaxKind.BadToken

            if (current.isWhitespace()) {
                while (current.isWhitespace()) {
                    position += 1
                }

                kind = SyntaxKind.WhitespaceToken
            } else if (current.isDigit()) {
                while (current.isDigit()) {
                    position += 1
                }

                kind = SyntaxKind.NumberToken
                currentText = lexer._text.substring(start until position)
                try {
                    value = currentText.toInt()
                } catch (_: NumberFormatException) {
                    lexer._diagnostics.add("The number $currentText doesn't fit in an Int")
                }
            } else when (current) {
                '\u0000' -> {
                    kind = SyntaxKind.EndOfFileToken
                }
                '+' -> {
                    kind = SyntaxKind.PlusToken
                    position += 1
                }
                '-' -> {
                    kind = SyntaxKind.MinusToken
                    position += 1
                }
                '*' -> {
                    kind = SyntaxKind.StarToken
                    position += 1
                }
                '/' -> {
                    kind = SyntaxKind.SlashToken
                    position += 1
                }
                '(' -> {
                    kind = SyntaxKind.OpenParenthesisToken
                    position += 1
                }
                ')' -> {
                    kind = SyntaxKind.CloseParenthesisToken
                    position += 1
                }
                else -> {}
            }

            if (kind == SyntaxKind.BadToken) {
                lexer._diagnostics.add("Unexpected character '$current'")
                position += 1
            }

            return SyntaxToken(kind, start, currentText ?: lexer._text.substring(start until position), value)
        }
    }
}
