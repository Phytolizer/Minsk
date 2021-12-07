using Minsk.CodeAnalysis;
using Minsk.CodeAnalysis.Syntax;

var showTree = false;

while (true)
{
    Console.Write("> ");
    var line = Console.ReadLine();
    if (string.IsNullOrWhiteSpace(line))
    {
        break;
    }

    switch (line)
    {
        case "#showTree":
            showTree = !showTree;
            Console.WriteLine(showTree ? "Showing parse tree" : "Not showing parse tree");
            continue;
        case "#cls":
            Console.Clear();
            continue;
    }

    var (expression, _, diagnostics) = SyntaxTree.Parse(line);

    if (showTree)
    {
        Console.ForegroundColor = ConsoleColor.DarkGray;
        SyntaxNode.PrettyPrint(expression);
        Console.ResetColor();
    }

    if (diagnostics.Any())
    {
        Console.ForegroundColor = ConsoleColor.DarkRed;
        foreach (var diagnostic in diagnostics)
        {
            Console.WriteLine(diagnostic);
        }

        Console.ResetColor();
    }
    else
    {
        var evaluator = new Evaluator(expression);
        Console.WriteLine(evaluator.Evaluate());
    }
}