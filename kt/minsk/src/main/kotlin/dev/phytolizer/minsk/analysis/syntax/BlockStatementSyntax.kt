package dev.phytolizer.minsk.analysis.syntax

class BlockStatementSyntax(
    val openBraceToken: SyntaxToken,
    val statements: List<StatementSyntax>,
    val closeBraceToken: SyntaxToken,
) : StatementSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.BlockStatement
    override val children: List<SyntaxNode>
        get() = listOf(listOf(openBraceToken), statements, listOf(closeBraceToken)).flatten()
}
