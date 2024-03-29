﻿using Minsk.CodeAnalysis.Syntax;
using Xunit;

namespace Minsk.Tests.CodeAnalysis.Syntax;

public class ParserTests
{
    [Theory]
    [MemberData(nameof(GetBinaryOperatorPairsData))]
    private void BinaryExpressionHonorsPrecedence(SyntaxKind op1, SyntaxKind op2)
    {
        var op1Precedence = SyntaxFacts.GetBinaryOperatorPrecedence(op1);
        var op2Precedence = SyntaxFacts.GetBinaryOperatorPrecedence(op2);
        var op1Text = SyntaxFacts.GetText(op1)!;
        var op2Text = SyntaxFacts.GetText(op2)!;
        var text = $"a {op1Text} b {op2Text} c";
        var expression = ParseExpression(text);

        if (op1Precedence >= op2Precedence)
        {
            using var e = new AssertingEnumerator(expression);

            e.AssertNode(SyntaxKind.BinaryExpression);
            e.AssertNode(SyntaxKind.BinaryExpression);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "a");
            e.AssertToken(op1, op1Text);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "b");
            e.AssertToken(op2, op2Text);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "c");
        }
        else
        {
            using var e = new AssertingEnumerator(expression);

            e.AssertNode(SyntaxKind.BinaryExpression);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "a");
            e.AssertToken(op1, op1Text);
            e.AssertNode(SyntaxKind.BinaryExpression);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "b");
            e.AssertToken(op2, op2Text);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "c");
        }
    }

    [Theory]
    [MemberData(nameof(GetUnaryOperatorPairsData))]
    private void UnaryExpressionHonorsPrecedences(SyntaxKind unaryKind, SyntaxKind binaryKind)
    {
        var unaryPrecedence = SyntaxFacts.GetUnaryOperatorPrecedence(unaryKind);
        var binaryPrecedence = SyntaxFacts.GetBinaryOperatorPrecedence(binaryKind);
        var unaryText = SyntaxFacts.GetText(unaryKind)!;
        var binaryText = SyntaxFacts.GetText(binaryKind)!;
        var text = $"{unaryText} a {binaryText} b";
        var expression = ParseExpression(text);

        if (unaryPrecedence >= binaryPrecedence)
        {
            using var e = new AssertingEnumerator(expression);

            e.AssertNode(SyntaxKind.BinaryExpression);
            e.AssertNode(SyntaxKind.UnaryExpression);
            e.AssertToken(unaryKind, unaryText);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "a");
            e.AssertToken(binaryKind, binaryText);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "b");
        }
        else
        {
            using var e = new AssertingEnumerator(expression);
            e.AssertNode(SyntaxKind.UnaryExpression);
            e.AssertToken(unaryKind, unaryText);
            e.AssertNode(SyntaxKind.BinaryExpression);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "a");
            e.AssertToken(binaryKind, binaryText);
            e.AssertNode(SyntaxKind.NameExpression);
            e.AssertToken(SyntaxKind.IdentifierToken, "b");
        }
    }

    private static IEnumerable<object[]> GetBinaryOperatorPairsData()
    {
        return SyntaxFacts.GetBinaryOperatorKinds()
            .SelectMany(_ => SyntaxFacts.GetBinaryOperatorKinds(),
                (op1, op2) => new object[] { op1, op2 });
    }

    private static IEnumerable<object[]> GetUnaryOperatorPairsData()
    {
        return SyntaxFacts.GetUnaryOperatorKinds()
            .SelectMany(_ => SyntaxFacts.GetBinaryOperatorKinds(),
                (unary, binary) => new object[] { unary, binary });
    }

    private static ExpressionSyntax ParseExpression(string text)
    {
        var statement = SyntaxTree.Parse(text).Root.Statement;
        return Assert.IsType<ExpressionStatementSyntax>(statement).Expression;
    }
}
