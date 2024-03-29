﻿using Minsk.CodeAnalysis;
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
    [InlineData("3 < 4", true)]
    [InlineData("5 < 4", false)]
    [InlineData("3 <= 4", true)]
    [InlineData("4 <= 4", true)]
    [InlineData("5 <= 4", false)]
    [InlineData("3 > 4", false)]
    [InlineData("5 > 4", true)]
    [InlineData("3 >= 4", false)]
    [InlineData("4 >= 4", true)]
    [InlineData("5 >= 4", true)]
    [InlineData("1 | 2", 3)]
    [InlineData("1 | 0", 1)]
    [InlineData("1 & 3", 1)]
    [InlineData("1 & 0", 0)]
    [InlineData("1 ^ 2", 3)]
    [InlineData("1 ^ 3", 2)]
    [InlineData("~1", -2)]
    [InlineData("false == false", true)]
    [InlineData("true == false", false)]
    [InlineData("false != false", false)]
    [InlineData("true != false", true)]
    [InlineData("true && true", true)]
    [InlineData("true && false", false)]
    [InlineData("false || false", false)]
    [InlineData("true || false", true)]
    [InlineData("false | false", false)]
    [InlineData("true | false", true)]
    [InlineData("false & false", false)]
    [InlineData("true & false", false)]
    [InlineData("false ^ false", false)]
    [InlineData("true ^ false", true)]
    [InlineData("true ^ true", false)]
    [InlineData("true", true)]
    [InlineData("false", false)]
    [InlineData("!true", false)]
    [InlineData("!false", true)]
    [InlineData("{ var a = 0 (a = 10) * a }", 100)]
    [InlineData("{ var a = 0 if a == 0 a = 10 a }", 10)]
    [InlineData("{ var a = 0 if a == 5 a = 10 a }", 0)]
    [InlineData("{ var a = 0 if a == 0 a = 10 else a = 20 a }", 10)]
    [InlineData("{ var a = 0 if a == 5 a = 10 else a = 20 a }", 20)]
    [InlineData("{ var i = 10 var result = 0 while i > 0 { result = result + i i = i - 1 } result }", 55)]
    [InlineData("{ var result = 0 for i = 1 to 10 { result = result + i } result }", 55)]
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

    [Fact]
    public void AssignmentExpressionReportsUndefined()
    {
        const string text = @"
            [x] = 10
        ";
        const string diagnostics = @"
            Undefined name 'x'
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void AssignmentExpressionReportsReadOnly()
    {
        const string text = @"
            {
                let x = 10
                x [=] 20
            }
        ";
        const string diagnostics = @"
            Cannot assign to read-only variable 'x'
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void AssignmentExpressionReportsCannotConvert()
    {
        const string text = @"
            {
                var x = 10
                x [=] true
            }
        ";
        const string diagnostics = @"
            Cannot convert type 'System.Boolean' to 'System.Int32'
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void UnaryOperatorReportsUndefined()
    {
        const string text = @"
            [+]true
        ";
        const string diagnostics = @"
            Unary operator '+' is not defined for type 'System.Boolean'
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void BinaryOperatorReportsUndefined()
    {
        const string text = @"
            true [+] true
        ";
        const string diagnostics = @"
            Binary operator '+' is not defined for types 'System.Boolean' and 'System.Boolean'
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void IfStatementReportsCannotConvert()
    {
        const string text = @"
            {
                var x = 0
                if [10]
                    x = 10
            }
        ";

        const string diagnostics = @"
            Cannot convert type 'System.Int32' to 'System.Boolean'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void BlockStatementNoInfiniteLoop()
    {
        const string text = @"
            {
            [)][]
        ";
        const string diagnostics = @"
            Expected next token to be IdentifierToken, got CloseParenthesisToken instead
            Expected next token to be CloseBraceToken, got EndOfFileToken instead
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void NameExpressionReportsNoErrorForInsertedToken()
    {
        const string text = "[]";
        const string diagnostics = @"
            Expected next token to be IdentifierToken, got EndOfFileToken instead
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void WhileStatementReportsCannotConvert()
    {
        const string text = @"
            {
                var x = 0
                while [10]
                    x = 10
            }
        ";

        const string diagnostics = @"
            Cannot convert type 'System.Int32' to 'System.Boolean'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void ForStatementReportsCannotConvertLowerBound()
    {
        const string text = @"
            {
                var result = 0
                for i = [false] to 10
                    result = result + i
            }
        ";
        const string diagnostics = @"
            Cannot convert type 'System.Boolean' to 'System.Int32'
        ";
        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void ForStatementReportsCannotConvertUpperBound()
    {
        const string text = @"
            {
                var result = 0
                for i = 1 to [true]
                    result = result + i
            }
        ";
        const string diagnostics = @"
            Cannot convert type 'System.Boolean' to 'System.Int32'
        ";
        AssertDiagnostics(text, diagnostics);
    }

    private static void AssertValue(string text, object expected)
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
