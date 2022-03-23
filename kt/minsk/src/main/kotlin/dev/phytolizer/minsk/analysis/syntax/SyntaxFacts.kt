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

    fun binaryOperators(): List<SyntaxKind> = SyntaxKind.values().filter { binaryOperatorPrecedence(it) > 0 }
    fun unaryOperators(): List<SyntaxKind> = SyntaxKind.values().filter { unaryOperatorPrecedence(it) > 0 }

    fun getText(kind: SyntaxKind): String? = when (kind) {
        SyntaxKind.PlusToken -> "+"
        SyntaxKind.MinusToken -> "-"
        SyntaxKind.StarToken -> "*"
        SyntaxKind.SlashToken -> "/"
        SyntaxKind.OpenParenthesisToken -> "("
        SyntaxKind.CloseParenthesisToken -> ")"
        SyntaxKind.OpenBraceToken -> "{"
        SyntaxKind.CloseBraceToken -> "}"
        SyntaxKind.BangToken -> "!"
        SyntaxKind.AmpersandAmpersandToken -> "&&"
        SyntaxKind.PipePipeToken -> "||"
        SyntaxKind.BangEqualsToken -> "!="
        SyntaxKind.EqualsEqualsToken -> "=="
        SyntaxKind.EqualsToken -> "="
        SyntaxKind.TrueKeyword -> "true"
        SyntaxKind.FalseKeyword -> "false"
        else -> null
    }
}
