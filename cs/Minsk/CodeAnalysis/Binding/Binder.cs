﻿using System.Collections.Immutable;
using Minsk.CodeAnalysis.Syntax;

namespace Minsk.CodeAnalysis.Binding;

internal sealed class Binder
{
    private readonly DiagnosticBag _diagnostics = new();
    private BoundScope _scope;

    public Binder(BoundScope? parent)
    {
        _scope = new BoundScope(parent);
    }

    public static BoundGlobalScope BindGlobalScope(BoundGlobalScope? previous, CompilationUnitSyntax syntax)
    {
        var parent = CreateParentScopes(previous);
        var binder = new Binder(parent);
        var statement = binder.BindStatement(syntax.Statement);
        var variables = binder._scope.GetDeclaredVariables();
        var diagnostics = binder.Diagnostics.ToImmutableArray();

        if (previous != null)
        {
            diagnostics = diagnostics.InsertRange(0, previous.Diagnostics);
        }

        return new BoundGlobalScope(null, diagnostics, variables, statement);
    }

    private BoundStatement BindStatement(StatementSyntax syntax)
    {
        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return syntax.Kind switch
        {
            SyntaxKind.BlockStatement => BindBlockStatement((BlockStatementSyntax)syntax),
            SyntaxKind.ExpressionStatement => BindExpressionStatement((ExpressionStatementSyntax)syntax),
            SyntaxKind.IfStatement => BindIfStatement((IfStatementSyntax)syntax),
            SyntaxKind.ForStatement => BindForStatement((ForStatementSyntax)syntax),
            SyntaxKind.VariableDeclaration => BindVariableDeclaration((VariableDeclarationSyntax)syntax),
            SyntaxKind.WhileStatement => BindWhileStatement((WhileStatementSyntax)syntax),
            _ => throw new InvalidOperationException(),
        };
    }

    private BoundStatement BindForStatement(ForStatementSyntax syntax)
    {
        var lowerBound = BindExpression(syntax.LowerBound, typeof(int));
        var upperBound = BindExpression(syntax.UpperBound, typeof(int));

        _scope = new BoundScope(_scope);

        var name = syntax.IdentifierToken.Text;
        var variable = new VariableSymbol(name, false, typeof(int));
        _scope.TryDeclare(variable);

        var body = BindStatement(syntax.Body);

        _scope = _scope.Parent!;

        return new BoundForStatement(variable, lowerBound, upperBound, body);
    }

    private BoundStatement BindWhileStatement(WhileStatementSyntax syntax)
    {
        var condition = BindExpression(syntax.Condition, typeof(bool));
        var body = BindStatement(syntax.Body);

        return new BoundWhileStatement(condition, body);
    }

    private BoundStatement BindIfStatement(IfStatementSyntax syntax)
    {
        var condition = BindExpression(syntax.Condition, typeof(bool));
        var thenStatement = BindStatement(syntax.ThenStatement);
        var elseStatement = syntax.ElseClause == null ? null : BindStatement(syntax.ElseClause.ElseStatement);

        return new BoundIfStatement(condition, thenStatement, elseStatement);
    }


    private BoundStatement BindVariableDeclaration(VariableDeclarationSyntax syntax)
    {
        var name = syntax.IdentifierToken.Text;
        var isReadOnly = syntax.KeywordToken.Kind == SyntaxKind.LetKeyword;
        var initializer = BindExpression(syntax.Initializer);
        var variable = new VariableSymbol(name, isReadOnly, initializer.Type);

        if (!_scope.TryDeclare(variable))
        {
            _diagnostics.ReportVariableAlreadyDeclared(syntax.IdentifierToken.Span, name);
        }

        return new BoundVariableDeclaration(variable, initializer);
    }

    private BoundStatement BindBlockStatement(BlockStatementSyntax syntax)
    {
        var statements = ImmutableArray.CreateBuilder<BoundStatement>();
        _scope = new BoundScope(_scope);

        foreach (var boundStatement in syntax.Statements.Select(BindStatement))
        {
            statements.Add(boundStatement);
        }

        _scope = _scope.Parent!;
        return new BoundBlockStatement(statements.ToImmutable());
    }

    private BoundStatement BindExpressionStatement(ExpressionStatementSyntax syntax)
    {
        var expression = BindExpression(syntax.Expression);
        return new BoundExpressionStatement(expression);
    }

    private static BoundScope? CreateParentScopes(BoundGlobalScope? previous)
    {
        var stack = new Stack<BoundGlobalScope>();
        while (previous != null)
        {
            stack.Push(previous);
            previous = previous.Previous;
        }

        BoundScope? current = null;

        while (stack.Count > 0)
        {
            var globalScope = stack.Pop();
            var scope = new BoundScope(current);
            foreach (var variable in globalScope.Variables)
            {
                scope.TryDeclare(variable);
            }

            current = scope;
        }

        return current;
    }

    public IEnumerable<Diagnostic> Diagnostics => _diagnostics;

    private BoundExpression BindExpression(ExpressionSyntax syntax, Type targetType)
    {
        var result = BindExpression(syntax);
        if (result.Type != targetType)
        {
            _diagnostics.ReportCannotConvert(syntax.Span, result.Type, targetType);
        }

        return result;
    }
    public BoundExpression BindExpression(ExpressionSyntax syntax)
    {
        // ReSharper disable once SwitchExpressionHandlesSomeKnownEnumValuesWithExceptionInDefault
        return syntax.Kind switch
        {
            SyntaxKind.BinaryExpression => BindBinaryExpression((BinaryExpressionSyntax)syntax),
            SyntaxKind.LiteralExpression => BindLiteralExpression((LiteralExpressionSyntax)syntax),
            SyntaxKind.UnaryExpression => BindUnaryExpression((UnaryExpressionSyntax)syntax),
            SyntaxKind.ParenthesizedExpression => BindParenthesizedExpression((ParenthesizedExpressionSyntax)syntax),
            SyntaxKind.NameExpression => BindNameExpression((NameExpressionSyntax)syntax),
            SyntaxKind.AssignmentExpression => BindAssignmentExpression((AssignmentExpressionSyntax)syntax),
            _ => throw new InvalidOperationException($"Unexpected syntax {syntax.Kind}")
        };
    }

    private BoundExpression BindNameExpression(NameExpressionSyntax syntax)
    {
        var name = syntax.IdentifierToken.Text;
        if (name == "")
        {
            // Token was inserted
            return new BoundLiteralExpression(0);
        }
        if (_scope.TryLookup(name, out var variable))
        {
            return new BoundVariableExpression(variable);
        }

        _diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name);
        return new BoundLiteralExpression(0);
    }

    private BoundExpression BindAssignmentExpression(AssignmentExpressionSyntax syntax)
    {
        var name = syntax.IdentifierToken.Text;
        var boundExpression = BindExpression(syntax.Expression);
        if (!_scope.TryLookup(name, out var variable))
        {
            _diagnostics.ReportUndefinedName(syntax.IdentifierToken.Span, name);
            return boundExpression;
        }

        if (variable.IsReadOnly)
        {
            _diagnostics.ReportCannotAssign(syntax.EqualsToken.Span, name);
            return boundExpression;
        }

        if (boundExpression.Type == variable.Type)
        {
            return new BoundAssignmentExpression(variable, boundExpression);
        }

        _diagnostics.ReportCannotConvert(syntax.EqualsToken.Span, boundExpression.Type, variable.Type);
        return boundExpression;
    }

    private BoundExpression BindParenthesizedExpression(ParenthesizedExpressionSyntax syntax)
    {
        return BindExpression(syntax.Expression);
    }

    private BoundExpression BindBinaryExpression(BinaryExpressionSyntax syntax)
    {
        var boundLeft = BindExpression(syntax.Left);
        var boundRight = BindExpression(syntax.Right);
        var boundOperator = BoundBinaryOperator.BindBinaryOperator(boundLeft, syntax.OperatorToken.Kind, boundRight);
        if (boundOperator != null)
        {
            return new BoundBinaryExpression(boundLeft, boundOperator, boundRight);
        }

        _diagnostics.ReportUndefinedBinaryOperator(
            syntax.OperatorToken.Span,
            syntax.OperatorToken,
            boundLeft.Type,
            boundRight.Type
        );
        return boundLeft;
    }

    private static BoundExpression BindLiteralExpression(LiteralExpressionSyntax syntax)
    {
        var value = syntax.Value;
        return new BoundLiteralExpression(value ?? 0);
    }

    private BoundExpression BindUnaryExpression(UnaryExpressionSyntax syntax)
    {
        var boundOperand = BindExpression(syntax.Operand);
        var boundOperator = BoundUnaryOperator.Bind(syntax.OperatorToken.Kind, boundOperand);
        if (boundOperator != null)
        {
            return new BoundUnaryExpression(boundOperator, boundOperand);
        }

        _diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Span, syntax.OperatorToken, boundOperand.Type);
        return boundOperand;
    }
}
