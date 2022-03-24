package dev.phytolizer.minsk.analysis.syntax

class ElseClauseSyntax(val elseKeyword: SyntaxToken, val elseStatement: StatementSyntax) : SyntaxNode() {
    override val kind: SyntaxKind
        get() = SyntaxKind.ElseClause
    override val children: List<SyntaxNode>
        get() = listOf(elseKeyword, elseStatement)
}
