namespace Minsk.CodeAnalysis.Syntax;

public sealed class VariableDeclarationSyntax : StatementSyntax
{
    public SyntaxToken KeywordToken { get; }
    public SyntaxToken IdentifierToken { get; }
    public SyntaxToken EqualsToken { get; }
    public ExpressionSyntax Initializer { get; }

    public VariableDeclarationSyntax(
        SyntaxToken keywordToken,
        SyntaxToken identifierToken,
        SyntaxToken equalsToken,
        ExpressionSyntax initializer
    )
    {
        KeywordToken = keywordToken;
        IdentifierToken = identifierToken;
        EqualsToken = equalsToken;
        Initializer = initializer;
    }

    public override SyntaxKind Kind => SyntaxKind.VariableDeclaration;

    public override IEnumerable<SyntaxNode> Children => new SyntaxNode[]
        { KeywordToken, IdentifierToken, EqualsToken, Initializer };
}
