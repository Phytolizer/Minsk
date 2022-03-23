package dev.phytolizer.minsk.analysis.syntax

class VariableDeclarationSyntax(
    val keywordToken: SyntaxToken,
    val identifierToken: SyntaxToken,
    val equalsToken: SyntaxToken,
    val initializer: ExpressionSyntax,
) : StatementSyntax() {
    override val kind: SyntaxKind
        get() = SyntaxKind.VariableDeclaration
    override val children: List<SyntaxNode>
        get() = listOf(keywordToken, identifierToken, equalsToken, initializer)
}
