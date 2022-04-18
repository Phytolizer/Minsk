using Minsk.CodeAnalysis.Syntax;
using Xunit;

namespace Minsk.Tests.CodeAnalysis.Syntax;

public sealed class SyntaxFactsTests
{
    [Theory]
    [MemberData(nameof(GetSyntaxKindsData))]
    private void GetTextRoundTrips(SyntaxKind kind)
    {
        var text = SyntaxFacts.GetText(kind)!;
        var tokens = SyntaxTree.ParseTokens(text);
        var token = Assert.Single(tokens);
        Assert.Equal(kind, token.Kind);
        Assert.Equal(text, token.Text);
    }

    private static IEnumerable<object[]> GetSyntaxKindsData()
    {
        return Enum.GetValues(typeof(SyntaxKind))
            .Cast<SyntaxKind>()
            .Where(kind => SyntaxFacts.GetText(kind) != null)
            .Select(kind => new object[] { kind });
    }
}
