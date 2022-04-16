using System.Text;
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
        var textBuilder = new StringBuilder();
        Compilation? previous = null;

        while (true)
        {
            Console.ForegroundColor = ConsoleColor.Green;
            if (textBuilder.Length == 0)
            {
                Console.Write("» ");
            }
            else
            {
                Console.Write("· ");
            }

            Console.ResetColor();

            var input = Console.ReadLine();
            if (input == null)
            {
                break;
            }

            var isBlank = input.All(char.IsWhiteSpace);

            if (textBuilder.Length == 0)
            {
                switch (input)
                {
                    case "#showTree":
                        showTree = !showTree;
                        Console.WriteLine(showTree ? "Showing parse trees." : "Not showing parse trees.");
                        continue;
                    case "#cls":
                        Console.Clear();
                        continue;
                    case "#reset":
                        previous = null;
                        continue;
                }
            }

            textBuilder.AppendLine(input);
            var text = textBuilder.ToString();

            var syntaxTree = SyntaxTree.Parse(text);

            if (!isBlank && syntaxTree.Diagnostics.Any())
            {
                continue;
            }

            var compilation = previous?.ContinueWith(syntaxTree) ?? new Compilation(syntaxTree);
            var result = compilation.Evaluate(variables);
            var diagnostics = result.Diagnostics.ToArray();
            if (diagnostics.Any())
            {
                foreach (var diagnostic in diagnostics)
                {
                    var lineIndex = syntaxTree.Text.GetLineIndex(diagnostic.Span.Start);
                    var lineNumber = lineIndex + 1;
                    var line = syntaxTree.Text.Lines[lineIndex];
                    var character = diagnostic.Span.Start - line.Start + 1;
                    Console.ForegroundColor = ConsoleColor.DarkRed;
                    Console.Write($"({lineNumber}, {character}): ");
                    Console.WriteLine(diagnostic);
                    Console.WriteLine();
                    var errorSpan = diagnostic.Span;
                    var prefixSpan = TextSpan.FromBounds(line.Start, errorSpan.Start);
                    var suffixSpan = TextSpan.FromBounds(errorSpan.End, line.End);
                    Console.ResetColor();
                    Console.Write("    ");
                    Console.Write(syntaxTree.Text.ToString(prefixSpan));
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.Write(syntaxTree.Text.ToString(errorSpan));
                    Console.ResetColor();
                    Console.WriteLine(syntaxTree.Text.ToString(suffixSpan));
                    Console.WriteLine();
                }

                Console.ResetColor();
            }
            else
            {
                previous = compilation;

                if (showTree)
                {
                    Console.ForegroundColor = ConsoleColor.DarkGray;
                    syntaxTree.Root.PrettyPrint();
                    Console.ResetColor();
                }

                Console.ForegroundColor = ConsoleColor.Magenta;
                Console.WriteLine(result.Value);
                Console.ResetColor();
            }

            textBuilder.Clear();
        }
    }
}
