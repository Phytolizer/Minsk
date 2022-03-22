using System.Collections.Immutable;
using Minsk.CodeAnalysis.Text;

namespace Minsk.CodeAnalysis.Syntax;

public sealed class SyntaxTree
{
    internal SyntaxTree(
        SourceText text,
        ExpressionSyntax root,
        SyntaxToken endOfFileToken,
        ImmutableArray<Diagnostic> diagnostics
    )
    {
        Text = text;
        Root = root;
        EndOfFileToken = endOfFileToken;
        Diagnostics = diagnostics;
    }

    public SourceText Text { get; }
    public ExpressionSyntax Root { get; }
    public SyntaxToken EndOfFileToken { get; }
    public ImmutableArray<Diagnostic> Diagnostics { get; }

    public static SyntaxTree Parse(string text)
    {
        var sourceText = SourceText.From(text);
        return Parse(sourceText);
    }

    public static SyntaxTree Parse(SourceText text)
    {
        var parser = new Parser(text);
        return parser.Parse();
    }

    public static IEnumerable<SyntaxToken> ParseTokens(string text)
    {
        var sourceText = SourceText.From(text);
        return ParseTokens(sourceText);
    }

    public static IEnumerable<SyntaxToken> ParseTokens(SourceText text)
    {
        return new Lexer(text);
    }
}
