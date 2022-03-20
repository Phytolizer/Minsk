using System.Text;

namespace Minsk.CodeAnalysis.Syntax;

public sealed class SyntaxToken : SyntaxNode
{
    public override SyntaxKind Kind { get; }
    public override IEnumerable<SyntaxNode> Children => Enumerable.Empty<SyntaxNode>();
    public string Text { get; }
    public int Position { get; }
    public object? Value { get; }

    public SyntaxToken(SyntaxKind kind, string text, int position, object? value)
    {
        Kind = kind;
        Text = text;
        Position = position;
        Value = value;
    }

    public override string ToString()
    {
        var result = new StringBuilder();
        result.Append($"{Kind} '{Text}'");
        if (Value != null)
        {
            result.Append($" {Value}");
        }

        return result.ToString();
    }
}