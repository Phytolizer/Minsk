package dev.phytolizer.minsk.analysis.syntax

class SyntaxTree(val root: ExpressionSyntax, val endOfFileToken: SyntaxToken, val diagnostics: List<String>) {
    companion object {
        fun parse(text: String): SyntaxTree {
            return Parser(text).parse()
        }
    }
}
