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
    var expression = parser.ParseExpression();

    Console.ForegroundColor = ConsoleColor.DarkGray;
    SyntaxNode.PrettyPrint(expression);
    Console.ResetColor();
}