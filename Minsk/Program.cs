using Minsk;

while (true)
{
    Console.Write("> ");
    var line = Console.ReadLine();
    if (string.IsNullOrWhiteSpace(line))
    {
        break;
    }

    var parser = new Parser(line);
    var (expression, _, diagnostics) = parser.Parse();


    Console.ForegroundColor = ConsoleColor.DarkGray;
    SyntaxNode.PrettyPrint(expression);
    Console.ResetColor();

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