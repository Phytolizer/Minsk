package dev.phytolizer.minsk.analysis.syntax

class ExpressionStatementSyntax(val expression: ExpressionSyntax) : StatementSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.ExpressionStatement
    override val children: List<SyntaxNode>
        get() = listOf(expression)
}
