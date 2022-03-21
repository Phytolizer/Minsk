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

            var syntaxTree = SyntaxTree.Parse(line);
            var compilation = new Compilation(syntaxTree);
            var result = compilation.Evaluate();
            var diagnostics = result.Diagnostics.ToArray();
            if (diagnostics.Any())
            {
                Console.ForegroundColor = ConsoleColor.DarkRed;
                foreach (var diagnostic in diagnostics) Console.WriteLine(diagnostic);

                Console.ResetColor();
            }
            else
            {
                if (showTree)
                {
                    Console.ForegroundColor = ConsoleColor.DarkGray;
                    syntaxTree.Root.PrettyPrint();
                    Console.ResetColor();
                }

                Console.WriteLine(result.Value);
            }
        }
    }
}