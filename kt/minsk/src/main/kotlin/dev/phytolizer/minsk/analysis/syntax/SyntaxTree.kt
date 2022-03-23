package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.Diagnostic
import dev.phytolizer.minsk.analysis.text.SourceText

class SyntaxTree private constructor(val text: SourceText) {
    val root: CompilationUnitSyntax
    val diagnostics: List<Diagnostic>

    init {
        val parser = Parser(text)
        root = parser.parseCompilationUnit()
        diagnostics = parser.diagnostics
    }

    companion object {
        fun parse(text: String): SyntaxTree {
            return parse(SourceText.from(text))
        }

        fun parse(text: SourceText): SyntaxTree {
            return SyntaxTree(text)
        }

        fun parseTokens(text: String): List<SyntaxToken> {
            return parseTokens(SourceText.from(text))
        }

        fun parseTokens(text: SourceText): List<SyntaxToken> {
            return Lexer(text).toList()
        }
    }
}
