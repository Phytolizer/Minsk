namespace Minsk.CodeAnalysis.Syntax;

internal static class SyntaxFacts
{
    public static int GetBinaryOperatorPrecedence(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.StarToken or SyntaxKind.SlashToken => 4,
            SyntaxKind.PlusToken or SyntaxKind.MinusToken => 3,
            SyntaxKind.AmpersandAmpersandToken => 2,
            SyntaxKind.PipePipeToken => 1,
            _ => 0,
        };
    }

    public static int GetUnaryOperatorPrecedence(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken or SyntaxKind.MinusToken => 5,
            _ => 0,
        };
    }

    public static SyntaxKind GetKeywordKind(string text)
    {
        return text switch
        {
            "true" => SyntaxKind.TrueKeyword,
            "false" => SyntaxKind.FalseKeyword,
            _ => SyntaxKind.IdentifierToken
        };
    }
}