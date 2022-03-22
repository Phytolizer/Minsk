package dev.phytolizer.minsk

class UnaryExpressionSyntax(val operatorToken: SyntaxToken, val operand: ExpressionSyntax) : ExpressionSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.UnaryExpression
    override val children: List<SyntaxNode>
        get() = listOf(operatorToken, operand)
}
