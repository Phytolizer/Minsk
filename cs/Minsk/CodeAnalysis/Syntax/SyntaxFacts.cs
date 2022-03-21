namespace Minsk.CodeAnalysis.Syntax;

public static class SyntaxFacts
{
    public static int GetBinaryOperatorPrecedence(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.StarToken or SyntaxKind.SlashToken => 5,
            SyntaxKind.PlusToken or SyntaxKind.MinusToken => 4,
            SyntaxKind.EqualsEqualsToken or SyntaxKind.BangEqualsToken => 3,
            SyntaxKind.AmpersandAmpersandToken => 2,
            SyntaxKind.PipePipeToken => 1,
            _ => 0
        };
    }

    public static int GetUnaryOperatorPrecedence(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken or SyntaxKind.MinusToken or SyntaxKind.BangToken => 6,
            _ => 0
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

    public static string? GetText(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.BangToken => "!",
            SyntaxKind.EqualsToken => "=",
            SyntaxKind.MinusToken => "-",
            SyntaxKind.PlusToken => "+",
            SyntaxKind.SlashToken => "/",
            SyntaxKind.StarToken => "*",
            SyntaxKind.AmpersandAmpersandToken => "&&",
            SyntaxKind.BangEqualsToken => "!=",
            SyntaxKind.PipePipeToken => "||",
            SyntaxKind.OpenParenthesisToken => "(",
            SyntaxKind.CloseParenthesisToken => ")",
            SyntaxKind.EqualsEqualsToken => "==",
            _ => null,
        };
    }

    public static IEnumerable<SyntaxKind> GetUnaryOperatorKinds()
    {
        return Enum.GetValues(typeof(SyntaxKind))
            .Cast<SyntaxKind>()
            .Where(kind => GetUnaryOperatorPrecedence(kind) > 0);
    }

    public static IEnumerable<SyntaxKind> GetBinaryOperatorKinds()
    {
        return Enum.GetValues(typeof(SyntaxKind))
            .Cast<SyntaxKind>()
            .Where(kind => GetBinaryOperatorPrecedence(kind) > 0);
    }
}