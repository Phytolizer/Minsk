using System.Text;

namespace Minsk;

public record SyntaxToken
{
    public SyntaxKind Kind { get; }
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
