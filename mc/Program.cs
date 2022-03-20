using Minsk.CodeAnalysis;
using Minsk.CodeAnalysis.Syntax;

namespace mc;

internal static class Program
{
    private static void Main()
    {
        var showTree = false;

        while (true)
        {
            Console.Write("> ");
            var line = Console.ReadLine();
            if (line == null)
            {
                break;
            }

            switch (line)
            {
                case "#showTree":
                    showTree = !showTree;
                    Console.WriteLine(showTree ? "Showing parse trees." : "Not showing parse trees.");
                    continue;
                case "#cls":
                    Console.Clear();
                    continue;
            }

            var parser = new Parser(line);
            var syntaxTree = parser.Parse();
            var diagnostics = syntaxTree.Diagnostics;
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
                if (showTree)
                {
                    syntaxTree.Root.PrettyPrint();
                }

                Console.WriteLine(new Evaluator(syntaxTree.Root).Evaluate());
            }
        }
    }
}