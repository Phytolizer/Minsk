package dev.phytolizer.minsk.analysis.syntax

class BinaryExpressionSyntax(val left: ExpressionSyntax, val operatorToken: SyntaxToken, val right: ExpressionSyntax) :
    ExpressionSyntax() {

    override val kind: SyntaxKind
        get() = SyntaxKind.BinaryExpression
    override val children: List<SyntaxNode>
        get() = listOf(left, operatorToken, right)
}
