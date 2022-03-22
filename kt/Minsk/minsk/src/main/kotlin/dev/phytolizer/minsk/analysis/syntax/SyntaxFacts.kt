package dev.phytolizer.minsk.analysis.syntax

object SyntaxFacts {
    fun binaryOperatorPrecedence(kind: SyntaxKind): Int = when (kind) {
        SyntaxKind.StarToken, SyntaxKind.SlashToken -> 2
        SyntaxKind.PlusToken, SyntaxKind.MinusToken -> 1
        else -> 0
    }

    fun unaryOperatorPrecedence(kind: SyntaxKind): Int = when (kind) {
        SyntaxKind.PlusToken, SyntaxKind.MinusToken -> 3
        else -> 0
    }
}
