using Minsk.CodeAnalysis.Syntax;
using Xunit;

namespace Minsk.Tests.CodeAnalysis.Syntax;

public sealed class LexerTests
{
    private sealed class SimpleToken
    {
        public SyntaxKind Kind { get; }
        public string Text { get; }

        public SimpleToken(SyntaxKind kind, string text)
        {
            Kind = kind;
            Text = text;
        }
    }

    private static IEnumerable<SimpleToken> GetTokens()
    {
        return new SimpleToken[]
        {
            new(SyntaxKind.IdentifierToken, "a"),
            new(SyntaxKind.IdentifierToken, "abc"),
            new(SyntaxKind.NumberToken, "1"),
            new(SyntaxKind.NumberToken, "123"),

            new(SyntaxKind.PlusToken, "+"),
            new(SyntaxKind.MinusToken, "-"),
            new(SyntaxKind.StarToken, "*"),
            new(SyntaxKind.SlashToken, "/"),
            new(SyntaxKind.BangToken, "!"),
            new(SyntaxKind.AmpersandAmpersandToken, "&&"),
            new(SyntaxKind.PipePipeToken, "||"),
            new(SyntaxKind.BangEqualsToken, "!="),
            new(SyntaxKind.EqualsEqualsToken, "=="),
            new(SyntaxKind.EqualsToken, "="),
            new(SyntaxKind.OpenParenthesisToken, "("),
            new(SyntaxKind.CloseParenthesisToken, ")"),

            new(SyntaxKind.TrueKeyword, "true"),
            new(SyntaxKind.FalseKeyword, "false"),
        };
    }

    private static IEnumerable<SimpleToken> GetSeparators()
    {
        return new SimpleToken[]
        {
            new(SyntaxKind.WhitespaceToken, " "),
            new(SyntaxKind.WhitespaceToken, "  "),
            new(SyntaxKind.WhitespaceToken, "\n"),
            new(SyntaxKind.WhitespaceToken, "\r"),
            new(SyntaxKind.WhitespaceToken, "\r\n"),
        };
    }

    private static IEnumerable<(SimpleToken t1, SimpleToken t2)> GetTokenPairs()
    {
        return GetTokens()
            .SelectMany(_ => GetTokens(), (t1, t2) => new { t1, t2 })
            .Where(elem => !RequiresSeparator(elem.t1.Kind, elem.t2.Kind))
            .Select(elem => (elem.t1, elem.t2));
    }

    private static bool RequiresSeparator(SyntaxKind t1Kind, SyntaxKind t2Kind)
    {
        var t1IsKeyword = t1Kind.ToString().EndsWith("Keyword");
        var t2IsKeyword = t2Kind.ToString().EndsWith("Keyword");

        if ((t1Kind == SyntaxKind.IdentifierToken || t1IsKeyword) &&
            (t2Kind == SyntaxKind.IdentifierToken || t2IsKeyword))
        {
            return true;
        }

        if ((t1Kind is SyntaxKind.NumberToken or SyntaxKind.IdentifierToken || t1IsKeyword) &&
            t2Kind == SyntaxKind.NumberToken)
        {
            return true;
        }

        if (t1Kind is SyntaxKind.BangToken or SyntaxKind.EqualsToken &&
            t2Kind is SyntaxKind.EqualsToken or SyntaxKind.EqualsEqualsToken)
        {
            return true;
        }

        return false;
    }

    private static IEnumerable<object[]> GetLexesTokenData()
    {
        return GetTokens().Select(t => new object[] { t.Kind, t.Text });
    }

    private static IEnumerable<object[]> GetLexesTokenPairsData()
    {
        return GetTokenPairs().Select(t => new object[] { t.t1, t.t2 });
    }

    [Theory]
    [MemberData(nameof(GetLexesTokenData))]
    private void LexesToken(SyntaxKind kind, string text)
    {
        var tokens = SyntaxTree.ParseTokens(text);
        var token = Assert.Single(tokens);
        Assert.Equal(kind, token.Kind);
        Assert.Equal(text, token.Text);
    }

    [Theory]
    [MemberData(nameof(GetLexesTokenPairsData))]
    private void LexesTokenPairs(SimpleToken t1, SimpleToken t2)
    {
        var text = t1.Text + t2.Text;
        var tokens = SyntaxTree.ParseTokens(text).ToArray();
        Assert.Equal(2, tokens.Length);
        Assert.Equal(t1.Kind, tokens[0].Kind);
        Assert.Equal(t1.Text, tokens[0].Text);
        Assert.Equal(t2.Kind, tokens[1].Kind);
        Assert.Equal(t2.Text, tokens[1].Text);
    }
}