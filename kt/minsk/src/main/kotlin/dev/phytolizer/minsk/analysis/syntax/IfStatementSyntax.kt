package dev.phytolizer.minsk.analysis.syntax

class IfStatementSyntax(
    val ifKeyword: SyntaxToken,
    val condition: ExpressionSyntax,
    val thenStatement: StatementSyntax,
    val elseClause: ElseClauseSyntax?,
) : StatementSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.IfStatement
    override val children: List<SyntaxNode>
        get() = listOfNotNull(ifKeyword, condition, thenStatement, elseClause)
}
