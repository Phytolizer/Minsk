package dev.phytolizer.minsk.analysis.syntax

class CompilationUnitSyntax(val statement: StatementSyntax, val endOfFileToken: SyntaxToken) : SyntaxNode() {
    override val kind: SyntaxKind
        get() = SyntaxKind.CompilationUnit
    override val children: List<SyntaxNode>
        get() = listOf(statement, endOfFileToken)
}
