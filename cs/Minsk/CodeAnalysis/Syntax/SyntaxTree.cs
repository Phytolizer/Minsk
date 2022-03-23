using System.Collections.Immutable;
using Minsk.CodeAnalysis.Text;

namespace Minsk.CodeAnalysis.Syntax;

public sealed class SyntaxTree
{
    private SyntaxTree(SourceText text)
    {
        Text = text;

        var parser = new Parser(text);
        Root = parser.ParseCompilationUnit();
        Diagnostics = parser.Diagnostics;
    }

    public SourceText Text { get; }
    public CompilationUnitSyntax Root { get; }
    public ImmutableArray<Diagnostic> Diagnostics { get; }

    public static SyntaxTree Parse(string text)
    {
        var sourceText = SourceText.From(text);
        return Parse(sourceText);
    }

    public static SyntaxTree Parse(SourceText text)
    {
        return new SyntaxTree(text);
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
