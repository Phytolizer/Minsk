using System.Collections.Immutable;
using System.Text;
using Minsk.CodeAnalysis.Text;

namespace Minsk.Tests.CodeAnalysis.Text;

internal sealed class AnnotatedText
{
    private AnnotatedText(string text, ImmutableArray<TextSpan> spans)
    {
        Text = text;
        Spans = spans;
    }

    public string Text { get; }
    public ImmutableArray<TextSpan> Spans { get; }

    public static AnnotatedText Parse(string text)
    {
        text = Unindent(text);

        var textBuilder = new StringBuilder();
        var spanBuilder = ImmutableArray.CreateBuilder<TextSpan>();
        var startStack = new Stack<int>();
        var position = 0;
        foreach (var c in text)
        {
            switch (c)
            {
                case '[':
                    startStack.Push(position);
                    break;
                case ']':
                    if (startStack.Count == 0)
                    {
                        throw new ArgumentException("Too many closing brackets in text", nameof(text));
                    }

                    var start = startStack.Pop();
                    var span = TextSpan.FromBounds(start, position);
                    spanBuilder.Add(span);
                    break;
                default:
                    position++;
                    textBuilder.Append(c);
                    break;
            }
        }

        if (startStack.Count != 0)
        {
            throw new ArgumentException("Too many opening brackets in text", nameof(text));
        }

        return new AnnotatedText(textBuilder.ToString(), spanBuilder.ToImmutable());
    }

    private static string Unindent(string text)
    {
        var lines = UnindentLines(text);
        return string.Join(Environment.NewLine, lines);
    }

    public static List<string> UnindentLines(string text)
    {
        var lines = new List<string>();
        using (var stringReader = new StringReader(text))
        {
            string? line;
            while ((line = stringReader.ReadLine()) != null)
            {
                lines.Add(line);
            }
        }

        if (lines.Count == 0)
        {
            return lines;
        }

        var minIndentation = lines.Where(line => line.Trim().Length != 0)
            .Select(line => line.Length - line.TrimStart().Length).Min();

        for (var i = 0; i < lines.Count; i++)
        {
            if (lines[i].Length < minIndentation)
            {
                lines[i] = "";
                continue;
            }

            lines[i] = lines[i][minIndentation..];
        }

        while (lines.Count > 0 && lines[0].Length == 0)
        {
            lines.RemoveAt(0);
        }

        while (lines.Count > 0 && lines[^1].Length == 0)
        {
            lines.RemoveAt(lines.Count - 1);
        }

        return lines;
    }
}
