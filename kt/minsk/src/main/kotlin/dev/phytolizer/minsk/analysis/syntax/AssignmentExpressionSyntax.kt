package dev.phytolizer.minsk.analysis.syntax

class AssignmentExpressionSyntax(
    val identifierToken: SyntaxToken,
    val equalsToken: SyntaxToken,
    val expression: ExpressionSyntax,
) : ExpressionSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.AssignmentExpression
    override val children: List<SyntaxNode>
        get() = listOf(identifierToken, equalsToken, expression)
}
