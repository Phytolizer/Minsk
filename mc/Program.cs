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
            var syntaxTree = parser.Parse();
            var diagnostics = syntaxTree.Diagnostics;
            if (diagnostics.Any())
            {
                foreach (var diagnostic in diagnostics)
                {
                    Console.WriteLine(diagnostic);
                }
            }
            else
            {
                Console.WriteLine(new Evaluator(syntaxTree.Root).Evaluate());
            }
        }
    }
}