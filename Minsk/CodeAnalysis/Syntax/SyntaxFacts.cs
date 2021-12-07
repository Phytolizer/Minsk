namespace Minsk.CodeAnalysis.Syntax;

public static class SyntaxFacts
{
    public static int GetBinaryOperatorPrecedence(this SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken or SyntaxKind.MinusToken => 1,
            SyntaxKind.StarToken or SyntaxKind.SlashToken => 2,
            _ => 0
        };
    }
}