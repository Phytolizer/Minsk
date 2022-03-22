package dev.phytolizer.minsk

class ParenthesizedExpressionSyntax(
    val openParenthesisToken: SyntaxToken,
    val expression: ExpressionSyntax,
    val closeParenthesisToken: SyntaxToken
) : ExpressionSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.ParenthesizedExpression
    override val children: List<SyntaxNode>
        get() = listOf(openParenthesisToken, expression, closeParenthesisToken)
}
