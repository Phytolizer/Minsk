using Minsk;

namespace mc;

internal static class Program
{
    private static void Main()
    {
        while (true)
        {
            Console.Write("> ");
            var line = Console.ReadLine();
            if (line == null)
            {
                break;
            }

            var parser = new Parser(line);
            var expression = parser.Parse();
            var diagnostics = parser.Diagnostics.ToArray();
            if (diagnostics.Any())
            {
                foreach (var diagnostic in diagnostics)
                {
                    Console.WriteLine(diagnostic);
                }
            }
            else
            {
                expression.PrettyPrint();
            }
        }
    }
}