package dev.phytolizer.minsk.analysis.syntax

object SyntaxFacts {
    fun binaryOperatorPrecedence(kind: SyntaxKind): Int = when (kind) {
        SyntaxKind.StarToken, SyntaxKind.SlashToken -> 5
        SyntaxKind.PlusToken, SyntaxKind.MinusToken -> 4

        SyntaxKind.EqualsEqualsToken, SyntaxKind.BangEqualsToken,
        SyntaxKind.LessToken, SyntaxKind.LessOrEqualsToken,
        SyntaxKind.GreaterToken, SyntaxKind.GreaterOrEqualsToken,
        -> 3

        SyntaxKind.AmpersandAmpersandToken -> 2
        SyntaxKind.PipePipeToken -> 1
        else -> 0
    }

    fun unaryOperatorPrecedence(kind: SyntaxKind): Int = when (kind) {
        SyntaxKind.PlusToken, SyntaxKind.MinusToken, SyntaxKind.BangToken -> 6
        else -> 0
    }

    fun keywordKind(text: String): SyntaxKind = when (text) {
        "else" -> SyntaxKind.ElseKeyword
        "false" -> SyntaxKind.FalseKeyword
        "if" -> SyntaxKind.IfKeyword
        "let" -> SyntaxKind.LetKeyword
        "true" -> SyntaxKind.TrueKeyword
        "var" -> SyntaxKind.VarKeyword
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
        SyntaxKind.LessToken -> "<"
        SyntaxKind.LessOrEqualsToken -> "<="
        SyntaxKind.GreaterToken -> ">"
        SyntaxKind.GreaterOrEqualsToken -> ">="
        SyntaxKind.ElseKeyword -> "else"
        SyntaxKind.FalseKeyword -> "false"
        SyntaxKind.IfKeyword -> "if"
        SyntaxKind.LetKeyword -> "let"
        SyntaxKind.TrueKeyword -> "true"
        SyntaxKind.VarKeyword -> "var"
        else -> null
    }
}
