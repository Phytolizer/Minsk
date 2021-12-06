namespace Minsk;

public class SyntaxToken : SyntaxNode
{
    public SyntaxToken(int Position, string Text, SyntaxKind Kind, object? Value = null)
    {
        this.Position = Position;
        this.Text = Text;
        this.Kind = Kind;
        this.Value = Value;
    }

    public int Position { get; }
    public string Text { get; }
    public override SyntaxKind Kind { get; }
    public object? Value { get; }

    public override IEnumerable<SyntaxNode> Children => Enumerable.Empty<SyntaxNode>();
}