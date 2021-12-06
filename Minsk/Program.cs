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
    var (expression, diagnostics) = parser.Parse();

    Console.ForegroundColor = ConsoleColor.Red;
    foreach (var diagnostic in diagnostics)
    {
        Console.WriteLine(diagnostic);
    }

    Console.ForegroundColor = ConsoleColor.DarkGray;
    SyntaxNode.PrettyPrint(expression);
    Console.ResetColor();
}