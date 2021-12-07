using System.Collections.Immutable;

namespace Minsk;

public record SyntaxTree(ExpressionSyntax Expression, SyntaxToken EndOfFileToken, ImmutableList<string> Diagnostics)
{
    public static SyntaxTree Parse(string text)
    {
        var parser = new Parser(text);
        return parser.Parse();
    }
}