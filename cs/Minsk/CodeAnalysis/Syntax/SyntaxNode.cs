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
        PrettyPrintInternal(this, true, "", true);
    }

    private static void PrettyPrintInternal(SyntaxNode node, bool isToConsole, string indent, bool isLast)
    {
        if (isToConsole)
        {
            Console.ForegroundColor = ConsoleColor.DarkGray;
        }

        Console.Write(indent);
        var marker = isLast ? "└───" : "├───";
        Console.Write(marker);

        if (isToConsole)
        {
            Console.ForegroundColor = node is SyntaxToken ? ConsoleColor.Blue : ConsoleColor.Cyan;
        }

        Console.Write(node.Kind);
        if (isToConsole)
        {
            Console.ResetColor();
        }

        if (node is SyntaxToken { Value: { } } t)
        {
            Console.Write($" {t.Value}");
        }

        Console.WriteLine();

        indent += isLast ? "    " : "│   ";
        var children = node.Children.ToArray();
        var lastChild = children.LastOrDefault();
        foreach (var child in children)
        {
            PrettyPrintInternal(child, isToConsole, indent, child == lastChild);
        }
    }
}
