package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.Diagnostic

class SyntaxTree(val root: ExpressionSyntax, val endOfFileToken: SyntaxToken, val diagnostics: List<Diagnostic>) {
    companion object {
        fun parse(text: String): SyntaxTree {
            return Parser(text).parse()
        }

        fun parseTokens(text: String): List<SyntaxToken> {
            return Lexer(text).toList()
        }
    }
}
