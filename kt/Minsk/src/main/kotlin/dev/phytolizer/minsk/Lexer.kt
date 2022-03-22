package dev.phytolizer.minsk

import java.lang.NumberFormatException

class Lexer(private val text: String) : Iterable<SyntaxToken> {
    override fun iterator(): Iterator<SyntaxToken> {
        return LexerIterator(this)
    }

    private class LexerIterator(private val lexer: Lexer) : Iterator<SyntaxToken> {
        private var position = 0
        private val current: Char
            get() {
                return if (position >= lexer.text.length) {
                    '\u0000'
                } else {
                    lexer.text[position]
                }
            }

        override fun hasNext(): Boolean {
            return position < lexer.text.length
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
                currentText = lexer.text.substring(start until position)
                try {
                    value = currentText.toInt()
                } catch (_: NumberFormatException) {
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
                position += 1
            }

            return SyntaxToken(kind, start, currentText ?: lexer.text.substring(start until position), value)
        }
    }
}
