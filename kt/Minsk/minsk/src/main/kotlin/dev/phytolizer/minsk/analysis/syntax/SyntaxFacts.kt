package dev.phytolizer.minsk.analysis.syntax

object SyntaxFacts {
    fun binaryOperatorPrecedence(kind: SyntaxKind): Int = when (kind) {
        SyntaxKind.StarToken, SyntaxKind.SlashToken -> 5
        SyntaxKind.PlusToken, SyntaxKind.MinusToken -> 4
        SyntaxKind.EqualsEqualsToken, SyntaxKind.BangEqualsToken -> 3
        SyntaxKind.AmpersandAmpersandToken -> 2
        SyntaxKind.PipePipeToken -> 1
        else -> 0
    }

    fun unaryOperatorPrecedence(kind: SyntaxKind): Int = when (kind) {
        SyntaxKind.PlusToken, SyntaxKind.MinusToken, SyntaxKind.BangToken -> 6
        else -> 0
    }

    fun keywordKind(text: String): SyntaxKind = when (text) {
        "true" -> SyntaxKind.TrueKeyword
        "false" -> SyntaxKind.FalseKeyword
        else -> SyntaxKind.IdentifierToken
    }
}
