package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.Diagnostic
import dev.phytolizer.minsk.analysis.text.SourceText

class SyntaxTree(
    val text: SourceText,
    val root: ExpressionSyntax,
    val endOfFileToken: SyntaxToken,
    val diagnostics: List<Diagnostic>,
) {
    companion object {
        fun parse(text: String): SyntaxTree {
            return parse(SourceText.from(text))
        }

        fun parse(text: SourceText): SyntaxTree {
            return Parser(text).parse()
        }

        fun parseTokens(text: String): List<SyntaxToken> {
            return parseTokens(SourceText.from(text))
        }

        fun parseTokens(text: SourceText): List<SyntaxToken> {
            return Lexer(text).toList()
        }
    }
}
