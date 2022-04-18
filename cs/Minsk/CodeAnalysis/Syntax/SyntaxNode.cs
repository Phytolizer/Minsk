using Minsk.CodeAnalysis.Text;

namespace Minsk.CodeAnalysis.Syntax;

public abstract class SyntaxNode
{
    public abstract SyntaxKind Kind { get; }
    public abstract IEnumerable<SyntaxNode> Children { get; }

    public virtual TextSpan Span
    {
        get
        {
            var first = Children.First().Span;
            var last = Children.Last().Span;
            return TextSpan.FromBounds(first.Start, last.End);
        }
    }

    public void PrettyPrint()
    {
        PrettyPrintInternal(this, Console.Out, true, "", true);
    }

    public void WriteTo(TextWriter writer)
    {
        PrettyPrintInternal(this, writer, false, "", true);
    }

    private static void PrettyPrintInternal(SyntaxNode node, TextWriter writer, bool isToConsole, string indent, bool isLast)
    {
        if (isToConsole)
        {
            Console.ForegroundColor = ConsoleColor.DarkGray;
        }

        writer.Write(indent);
        var marker = isLast ? "└───" : "├───";
        writer.Write(marker);

        if (isToConsole)
        {
            Console.ForegroundColor = node is SyntaxToken ? ConsoleColor.Blue : ConsoleColor.Cyan;
        }

        writer.Write(node.Kind);
        if (isToConsole)
        {
            Console.ResetColor();
        }

        if (node is SyntaxToken { Value: { } } t)
        {
            writer.Write($" {t.Value}");
        }

        writer.WriteLine();

        indent += isLast ? "    " : "│   ";
        var children = node.Children.ToArray();
        var lastChild = children.LastOrDefault();
        foreach (var child in children)
        {
            PrettyPrintInternal(child, writer, isToConsole, indent, child == lastChild);
        }
    }
}
