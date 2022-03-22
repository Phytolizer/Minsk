package dev.phytolizer.minsk.analysis.syntax

class NameExpressionSyntax(val identifierToken: SyntaxToken) : ExpressionSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.NameExpression
    override val children: List<SyntaxNode>
        get() = listOf(identifierToken)
}

