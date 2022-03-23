package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.Diagnostic
import dev.phytolizer.minsk.analysis.DiagnosticBag
import dev.phytolizer.minsk.analysis.text.SourceText
import dev.phytolizer.minsk.analysis.text.TextSpan

internal class Lexer(private val _text: SourceText) : Iterable<SyntaxToken> {
    private val _diagnostics = DiagnosticBag()
    val diagnostics: List<Diagnostic>
        get() = _diagnostics.toList()

    override fun iterator(): Iterator<SyntaxToken> {
        return LexerIterator(this)
    }

    private class LexerIterator(private val lexer: Lexer) : Iterator<SyntaxToken> {
        private var position = 0
        private fun peek(offset: Int): Char {
            val index = position + offset
            return if (index >= lexer._text.length) {
                '\u0000'
            } else {
                lexer._text[index]
            }
        }

        private val current: Char
            get() = peek(0)

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
                currentText = lexer._text.toString(start until position)
                try {
                    value = currentText.toInt()
                } catch (_: NumberFormatException) {
                    lexer._diagnostics.reportInvalidInt(TextSpan(start, position - start), currentText, Int::class)
                }
            } else if (current.isLetter()) {
                while (current.isLetterOrDigit()) {
                    position += 1
                }

                currentText = lexer._text.toString(start until position)
                kind = SyntaxFacts.keywordKind(currentText)
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
                '!' -> if (peek(1) == '=') {
                    kind = SyntaxKind.BangEqualsToken
                    position += 2
                } else {
                    kind = SyntaxKind.BangToken
                    position += 1
                }
                '&' -> if (peek(1) == '&') {
                    kind = SyntaxKind.AmpersandAmpersandToken
                    position += 2
                }
                '|' -> if (peek(1) == '|') {
                    kind = SyntaxKind.PipePipeToken
                    position += 2
                }
                '=' -> if (peek(1) == '=') {
                    kind = SyntaxKind.EqualsEqualsToken
                    position += 2
                } else {
                    kind = SyntaxKind.EqualsToken
                    position += 1
                }
                else -> {}
            }

            if (kind == SyntaxKind.BadToken) {
                lexer._diagnostics.reportBadCharacter(start, current)
                position += 1
            }

            return SyntaxToken(kind, start, currentText ?: lexer._text.toString(start until position), value)
        }
    }
}
