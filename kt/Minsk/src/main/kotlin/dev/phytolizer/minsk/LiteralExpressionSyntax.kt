package dev.phytolizer.minsk

class LiteralExpressionSyntax(val literalToken: SyntaxToken) : ExpressionSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.LiteralExpression
    override val children: List<SyntaxNode>
        get() = listOf(literalToken)
}
