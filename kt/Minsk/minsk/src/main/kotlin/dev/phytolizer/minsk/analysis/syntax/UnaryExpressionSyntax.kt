package dev.phytolizer.minsk.analysis.syntax

class UnaryExpressionSyntax(val operatorToken: SyntaxToken, val operand: ExpressionSyntax) : ExpressionSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.UnaryExpression
    override val children: List<SyntaxNode>
        get() = listOf(operatorToken, operand)
}
