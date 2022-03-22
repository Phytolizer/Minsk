using Minsk.CodeAnalysis.Syntax;
using Xunit;

namespace Minsk.Tests.CodeAnalysis.Syntax;

public sealed class LexerTests
{
    private static IEnumerable<SimpleToken> GetTokens()
    {
        var fixedTokens = Enum.GetValues(typeof(SyntaxKind))
            .Cast<SyntaxKind>()
            .Select(k => (kind: k, text: SyntaxFacts.GetText(k)))
            .Where(t => t.text != null)
            .Select(t => new SimpleToken(t.kind, t.text!));

        var dynamicTokens = new SimpleToken[]
        {
            new(SyntaxKind.IdentifierToken, "a"),
            new(SyntaxKind.IdentifierToken, "abc"),
            new(SyntaxKind.NumberToken, "1"),
            new(SyntaxKind.NumberToken, "123")
        };

        return fixedTokens.Concat(dynamicTokens);
    }

    private static IEnumerable<SimpleToken> GetSeparators()
    {
        return new SimpleToken[]
        {
            new(SyntaxKind.WhitespaceToken, " "),
            new(SyntaxKind.WhitespaceToken, "  "),
            new(SyntaxKind.WhitespaceToken, "\n"),
            new(SyntaxKind.WhitespaceToken, "\r"),
            new(SyntaxKind.WhitespaceToken, "\r\n")
        };
    }

    private static IEnumerable<(SimpleToken t1, SimpleToken t2)> GetTokenPairs()
    {
        return GetTokens()
            .SelectMany(_ => GetTokens(), (t1, t2) => new { t1, t2 })
            .Where(elem => !RequiresSeparator(elem.t1.Kind, elem.t2.Kind))
            .Select(elem => (elem.t1, elem.t2));
    }

    private static IEnumerable<(SimpleToken t1, SimpleToken separator, SimpleToken t2)> GetTokenPairsWithSeparator()
    {
        return GetTokens()
            .SelectMany(_ => GetTokens(), (t1, t2) => new { t1, t2 })
            .Where(elem => RequiresSeparator(elem.t1.Kind, elem.t2.Kind))
            .SelectMany(_ => GetSeparators(), (elem, separator) => (elem.t1, separator, elem.t2));
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
        return GetTokens().Select(t => new object[] { t });
    }

    private static IEnumerable<object[]> GetLexesTokenPairsData()
    {
        return GetTokenPairs().Select(t => new object[] { t.t1, t.t2 });
    }

    private static IEnumerable<object[]> GetLexesTokenPairsWithSeparatorData()
    {
        return GetTokenPairsWithSeparator().Select(t => new object[] { t.t1, t.separator, t.t2 });
    }

    [Fact]
    private void TestsAllTokens()
    {
        var tokenKinds = Enum.GetValues(typeof(SyntaxKind))
            .Cast<SyntaxKind>()
            .Where(k => k.ToString().EndsWith("Keyword") || k.ToString().EndsWith("Token"));

        var testedTokenKinds = GetTokens().Concat(GetSeparators()).Select(t => t.Kind);

        var untestedTokenKinds = new SortedSet<SyntaxKind>(tokenKinds);
        untestedTokenKinds.Remove(SyntaxKind.BadToken);
        untestedTokenKinds.Remove(SyntaxKind.EndOfFileToken);
        untestedTokenKinds.ExceptWith(testedTokenKinds);

        Assert.Empty(untestedTokenKinds);
    }

    [Theory]
    [MemberData(nameof(GetLexesTokenData))]
    private void LexesToken(SimpleToken t)
    {
        var tokens = SyntaxTree.ParseTokens(t.Text);
        var token = Assert.Single(tokens);
        Assert.Equal(t.Kind, token.Kind);
        Assert.Equal(t.Text, token.Text);
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

    [Theory]
    [MemberData(nameof(GetLexesTokenPairsWithSeparatorData))]
    private void LexesTokenPairsWithSeparator(SimpleToken t1, SimpleToken separator, SimpleToken t2)
    {
        var text = t1.Text + separator.Text + t2.Text;
        var tokens = SyntaxTree.ParseTokens(text).ToArray();
        Assert.Equal(3, tokens.Length);
        Assert.Equal(t1.Kind, tokens[0].Kind);
        Assert.Equal(t1.Text, tokens[0].Text);
        Assert.Equal(separator.Kind, tokens[1].Kind);
        Assert.Equal(separator.Text, tokens[1].Text);
        Assert.Equal(t2.Kind, tokens[2].Kind);
        Assert.Equal(t2.Text, tokens[2].Text);
    }

    private sealed class SimpleToken
    {
        public SimpleToken(SyntaxKind kind, string text)
        {
            Kind = kind;
            Text = text;
        }

        public SyntaxKind Kind { get; }
        public string Text { get; }
    }
}
