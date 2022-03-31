namespace Minsk.CodeAnalysis.Syntax;

public static class SyntaxFacts
{
    public static int GetBinaryOperatorPrecedence(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.StarToken or SyntaxKind.SlashToken => 5,
            SyntaxKind.PlusToken or SyntaxKind.MinusToken => 4,
            SyntaxKind.EqualsEqualsToken or SyntaxKind.BangEqualsToken
                or SyntaxKind.LessToken or SyntaxKind.LessEqualsToken
                or SyntaxKind.GreaterToken or SyntaxKind.GreaterEqualsToken => 3,
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
            "false" => SyntaxKind.FalseKeyword,
            "let" => SyntaxKind.LetKeyword,
            "true" => SyntaxKind.TrueKeyword,
            "var" => SyntaxKind.VarKeyword,
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
            SyntaxKind.LessToken => "<",
            SyntaxKind.LessEqualsToken => "<=",
            SyntaxKind.GreaterToken => ">",
            SyntaxKind.GreaterEqualsToken => ">=",
            SyntaxKind.PipePipeToken => "||",
            SyntaxKind.OpenParenthesisToken => "(",
            SyntaxKind.CloseParenthesisToken => ")",
            SyntaxKind.OpenBraceToken => "{",
            SyntaxKind.CloseBraceToken => "}",
            SyntaxKind.EqualsEqualsToken => "==",
            SyntaxKind.FalseKeyword => "false",
            SyntaxKind.LetKeyword => "let",
            SyntaxKind.TrueKeyword => "true",
            SyntaxKind.VarKeyword => "var",
            _ => null
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
