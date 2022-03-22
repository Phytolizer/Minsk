namespace Minsk.CodeAnalysis.Text;

public sealed record TextLine(SourceText Text, int Start, int Length, int LengthIncludingLineBreak)
{
    public int End => Start + Length;
    public TextSpan Span => new(Start, Length);
    public TextSpan SpanIncludingLineBreak => new(Start, LengthIncludingLineBreak);

    public override string ToString()
    {
        return Text.ToString(Span);
    }
}
