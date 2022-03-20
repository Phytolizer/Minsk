namespace Minsk;

public abstract class SyntaxNode
{
    public abstract SyntaxKind Kind { get; }
    public abstract IEnumerable<SyntaxNode> Children { get; }

    public void PrettyPrint()
    {
        PrettyPrintInternal(this, "", true);
    }

    private static void PrettyPrintInternal(SyntaxNode node, string indent, bool isLast)
    {
        Console.Write(indent);
        var marker = isLast ? "└───" : "├───";
        Console.Write(marker);
        if (node is SyntaxToken t)
        {
            Console.Write(t);
        }
        else
        {
            Console.Write(node.Kind);
        }

        Console.WriteLine();

        indent += isLast ? "    " : "│   ";
        var children = node.Children.ToArray();
        var lastChild = children.LastOrDefault();
        foreach (var child in children)
        {
            PrettyPrintInternal(child, indent, child == lastChild);
        }
    }
}