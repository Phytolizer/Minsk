using Minsk.CodeAnalysis.Text;
using Xunit;

namespace Minsk.Tests.CodeAnalysis.Text;

public sealed class SourceTextTests
{
    [Theory]
    [InlineData(".", 1)]
    [InlineData(".\r\n", 2)]
    [InlineData(".\r\n\r\n", 3)]
    private void ReportsCorrectLineCount(string text, int expectedLineCount)
    {
        var sourceText = SourceText.From(text);
        Assert.Equal(expectedLineCount, sourceText.Lines.Length);
    }
}
