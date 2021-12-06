using Minsk;

while (true)
{
    Console.Write("> ");
    var line = Console.ReadLine();
    if (string.IsNullOrWhiteSpace(line))
    {
        break;
    }

    var lexer = new Lexer(line);
    for (var token = lexer.NextToken(); token.Kind != SyntaxKind.EndOfFileToken; token = lexer.NextToken())
    {
        Console.Write($"{token.Kind}: '{token.Text}'");
        if (token.Value != null)
        {
            Console.Write($" {token.Value}");
        }
        Console.WriteLine();
    }
}