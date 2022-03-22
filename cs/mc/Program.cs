using Minsk.CodeAnalysis;
using Minsk.CodeAnalysis.Syntax;
using Minsk.CodeAnalysis.Text;

namespace mc;

internal static class Program
{
    private static void Main()
    {
        var showTree = false;
        var variables = new Dictionary<VariableSymbol, object>();

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
            var result = compilation.Evaluate(variables);
            var diagnostics = result.Diagnostics.ToArray();
            if (diagnostics.Any())
            {
                var text = syntaxTree.Text;

                foreach (var diagnostic in diagnostics)
                {
                    var lineIndex = text.GetLineIndex(diagnostic.Span.Start);
                    var lineNumber = lineIndex + 1;
                    var character = diagnostic.Span.Start - text.Lines[lineIndex].Start + 1;
                    Console.ForegroundColor = ConsoleColor.DarkRed;
                    Console.Write($"({lineNumber}, {character}): ");
                    Console.WriteLine(diagnostic);
                    Console.WriteLine();
                    var errorSpan = diagnostic.Span;
                    var prefixSpan = new TextSpan(0, errorSpan.Start);
                    var suffixSpan = TextSpan.FromBounds(errorSpan.End, line.Length);
                    Console.ResetColor();
                    Console.Write("    ");
                    Console.Write(line[prefixSpan.Start..prefixSpan.End]);
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.Write(line[errorSpan.Start..errorSpan.End]);
                    Console.ResetColor();
                    Console.WriteLine(line[suffixSpan.Start..suffixSpan.End]);
                    Console.WriteLine();
                }

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
