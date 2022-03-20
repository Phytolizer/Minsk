namespace Minsk.CodeAnalysis.Syntax;

internal static class SyntaxFacts
{
    public static int GetBinaryOperatorPrecedence(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.StarToken or SyntaxKind.SlashToken => 2,
            SyntaxKind.PlusToken or SyntaxKind.MinusToken => 1,
            _ => 0,
        };
    }

    public static int GetUnaryOperatorPrecedence(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken or SyntaxKind.MinusToken => 3,
            _ => 0,
        };
    }
}