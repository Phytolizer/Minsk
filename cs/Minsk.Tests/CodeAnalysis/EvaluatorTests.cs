using Minsk.CodeAnalysis;
using Minsk.CodeAnalysis.Syntax;
using Minsk.Tests.CodeAnalysis.Text;
using Xunit;

namespace Minsk.Tests.CodeAnalysis;

public sealed class EvaluatorTests
{
    [Theory]
    [InlineData("1", 1)]
    [InlineData("+1", 1)]
    [InlineData("-1", -1)]
    [InlineData("14 + 12", 26)]
    [InlineData("12 - 3", 9)]
    [InlineData("4 * 2", 8)]
    [InlineData("9 / 3", 3)]
    [InlineData("(10)", 10)]
    [InlineData("12 == 3", false)]
    [InlineData("3 == 3", true)]
    [InlineData("12 != 3", true)]
    [InlineData("3 != 3", false)]
    [InlineData("false == false", true)]
    [InlineData("true == false", false)]
    [InlineData("false != false", false)]
    [InlineData("true != false", true)]
    [InlineData("true", true)]
    [InlineData("false", false)]
    [InlineData("!true", false)]
    [InlineData("!false", true)]
    [InlineData("{ var a = 0 (a = 10) * a }", 100)]
    public void EvaluatesCorrectValue(string text, object expected)
    {
        AssertValue(text, expected);
    }

    [Fact]
    public void VariableDeclarationReportsRedeclaration()
    {
        const string text = @"
            {
                var x = 10
                var y = 100
                {
                    var x = 10
                }
                var [x] = 5
            }
        ";
        const string diagnostics = @"
            Name 'x' is already declared in this scope
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void NameExpressionReportsUndefined()
    {
        const string text = @"
            [x] * 10
        ";
        const string diagnostics = @"
            Undefined name 'x'
        ";
        AssertDiagnostics(text, diagnostics);
    }

    private void AssertValue(string text, object expected)
    {
        var syntaxTree = SyntaxTree.Parse(text);
        var compilation = new Compilation(syntaxTree);
        var variables = new Dictionary<VariableSymbol, object>();
        var result = compilation.Evaluate(variables);

        Assert.Empty(result.Diagnostics);
        Assert.Equal(expected, result.Value);
    }

    private static void AssertDiagnostics(string text, string diagnosticText)
    {
        var annotatedText = AnnotatedText.Parse(text);
        var syntaxTree = SyntaxTree.Parse(annotatedText.Text);
        var compilation = new Compilation(syntaxTree);
        var variables = new Dictionary<VariableSymbol, object>();
        var result = compilation.Evaluate(variables);
        var diagnostics = AnnotatedText.UnindentLines(diagnosticText);

        Assert.Equal(diagnostics.Count, annotatedText.Spans.Length);
        Assert.Equal(diagnostics.Count, result.Diagnostics.Length);

        for (var i = 0; i < diagnostics.Count; i++)
        {
            var expectedMessage = diagnostics[i];
            var actualMessage = result.Diagnostics[i].Message;

            Assert.Equal(expectedMessage, actualMessage);

            var expectedSpan = annotatedText.Spans[i];
            var actualSpan = result.Diagnostics[i].Span;

            Assert.Equal(expectedSpan, actualSpan);
        }
    }
}
