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
            "else" => SyntaxKind.ElseKeyword,
            "false" => SyntaxKind.FalseKeyword,
            "for" => SyntaxKind.ForKeyword,
            "if" => SyntaxKind.IfKeyword,
            "let" => SyntaxKind.LetKeyword,
            "to" => SyntaxKind.ToKeyword,
            "true" => SyntaxKind.TrueKeyword,
            "var" => SyntaxKind.VarKeyword,
            "while" => SyntaxKind.WhileKeyword,
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
            SyntaxKind.ElseKeyword => "else",
            SyntaxKind.FalseKeyword => "false",
            SyntaxKind.ForKeyword => "for",
            SyntaxKind.IfKeyword => "if",
            SyntaxKind.LetKeyword => "let",
            SyntaxKind.ToKeyword => "to",
            SyntaxKind.TrueKeyword => "true",
            SyntaxKind.VarKeyword => "var",
            SyntaxKind.WhileKeyword => "while",
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
