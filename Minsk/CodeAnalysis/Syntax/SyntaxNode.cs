namespace Minsk.CodeAnalysis.Syntax;

public abstract class SyntaxNode
{
    public abstract SyntaxKind Kind { get; }
    public abstract IEnumerable<SyntaxNode> Children { get; }

    public static void PrettyPrint(SyntaxNode node, string indent = "")
    {
        Console.Write($"{indent}{node.Kind}");

        if (node is SyntaxToken { Value: { } } t)
        {
            Console.Write($" {t.Value}");
        }

        Console.WriteLine();

        indent += "    ";

        foreach (var child in node.Children)
        {
            PrettyPrint(child, indent);
        }
    }
}