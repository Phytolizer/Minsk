package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.Diagnostic
import dev.phytolizer.minsk.analysis.DiagnosticBag
import dev.phytolizer.minsk.analysis.VariableSymbol
import dev.phytolizer.minsk.analysis.syntax.*
import kotlin.reflect.KClass

internal class Binder(parent: BoundScope?) {
    private var _scope = BoundScope(parent)
    private val _diagnostics = DiagnosticBag()
    val diagnostics: List<Diagnostic>
        get() = _diagnostics.toList()

    companion object {
        fun bindGlobalScope(previous: BoundGlobalScope?, syntax: CompilationUnitSyntax): BoundGlobalScope {
            val parentScope = createParentScopes(previous)
            val binder = Binder(parentScope)
            val statement = binder.bindStatement(syntax.statement)
            val variables = binder._scope.declaredVariables()
            var diagnostics = binder.diagnostics

            if (previous != null) {
                diagnostics = listOf(previous.diagnostics, diagnostics).flatten()
            }

            return BoundGlobalScope(previous, diagnostics, variables, statement)
        }

        private fun createParentScopes(previousIn: BoundGlobalScope?): BoundScope? {
            val stack = mutableListOf<BoundGlobalScope>()
            var previous = previousIn

            while (previous != null) {
                stack.add(previous)
                previous = previous.previous
            }

            var current: BoundScope? = null

            while (stack.isNotEmpty()) {
                previous = stack.removeLast()
                val scope = BoundScope(current)
                for (v in previous.variables) {
                    scope.tryDeclare(v)
                }
                current = scope
            }

            return current
        }
    }

    fun bindStatement(syntax: StatementSyntax): BoundStatement = when (syntax.kind) {
        SyntaxKind.ExpressionStatement -> bindExpressionStatement(syntax as ExpressionStatementSyntax)
        SyntaxKind.BlockStatement -> bindBlockStatement(syntax as BlockStatementSyntax)
        SyntaxKind.IfStatement -> bindIfStatement(syntax as IfStatementSyntax)
        SyntaxKind.VariableDeclaration -> bindVariableDeclaration(syntax as VariableDeclarationSyntax)
        else -> throw IllegalStateException()
    }

    private fun bindIfStatement(syntax: IfStatementSyntax): BoundStatement {
        val condition = bindExpression(syntax.condition, Boolean::class)
        val thenStatement = bindStatement(syntax.thenStatement)
        val elseStatement = if (syntax.elseClause == null) {
            null
        } else {
            bindStatement(syntax.elseClause.elseStatement)
        }
        return BoundIfStatement(condition, thenStatement, elseStatement)
    }

    private fun bindBlockStatement(syntax: BlockStatementSyntax): BoundStatement {
        val statements = mutableListOf<BoundStatement>()
        _scope = BoundScope(_scope)

        for (statement in syntax.statements) {
            statements.add(bindStatement(statement))
        }

        _scope = _scope.parent!!
        return BoundBlockStatement(statements)
    }

    private fun bindExpressionStatement(syntax: ExpressionStatementSyntax): BoundStatement {
        val expression = bindExpression(syntax.expression)
        return BoundExpressionStatement(expression)
    }

    private fun bindVariableDeclaration(syntax: VariableDeclarationSyntax): BoundStatement {
        val name = syntax.identifierToken.text
        val isReadOnly = syntax.keywordToken.kind == SyntaxKind.LetKeyword
        val initializer = bindExpression(syntax.initializer)
        val variable = VariableSymbol(name, isReadOnly, initializer.type)

        if (!_scope.tryDeclare(variable)) {
            _diagnostics.reportVariableAlreadyDeclared(syntax.identifierToken.span, name)
        }

        return BoundVariableDeclaration(variable, initializer)
    }

    private fun bindExpression(syntax: ExpressionSyntax, targetType: KClass<Boolean>? = null): BoundExpression {
        val result = when (syntax.kind) {
            SyntaxKind.AssignmentExpression -> bindAssignmentExpression(syntax as AssignmentExpressionSyntax)
            SyntaxKind.BinaryExpression -> bindBinaryExpression(syntax as BinaryExpressionSyntax)
            SyntaxKind.LiteralExpression -> bindLiteralExpression(syntax as LiteralExpressionSyntax)
            SyntaxKind.NameExpression -> bindNameExpression(syntax as NameExpressionSyntax)
            SyntaxKind.ParenthesizedExpression -> bindParenthesizedExpression(syntax as ParenthesizedExpressionSyntax)
            SyntaxKind.UnaryExpression -> bindUnaryExpression(syntax as UnaryExpressionSyntax)
            else -> throw IllegalStateException()
        }
        if (targetType != null && result.type != targetType) {
            _diagnostics.reportCannotConvert(syntax.span, result.type, targetType)
        }
        return result
    }

    private fun bindAssignmentExpression(syntax: AssignmentExpressionSyntax): BoundExpression {
        val expression = bindExpression(syntax.expression)

        val name = syntax.identifierToken.text
        val variable = _scope.tryLookup(name)
        if (variable == null) {
            _diagnostics.reportUndefinedName(syntax.identifierToken.span, name)
            return expression
        }

        if (variable.isReadOnly) {
            _diagnostics.reportCannotAssign(syntax.equalsToken.span, name)
        }

        if (variable.type != expression.type) {
            _diagnostics.reportCannotConvert(syntax.equalsToken.span, expression.type, variable.type)
            return expression
        }

        return BoundAssignmentExpression(variable, expression)
    }

    private fun bindBinaryExpression(syntax: BinaryExpressionSyntax): BoundExpression {
        val left = bindExpression(syntax.left)
        val right = bindExpression(syntax.right)

        val op = BoundBinaryOperator.bind(syntax.operatorToken.kind, left.type, right.type)
        return if (op == null) {
            _diagnostics.reportUndefinedBinaryOperator(
                syntax.operatorToken.span,
                syntax.operatorToken.text,
                left.type,
                right.type,
            )
            left
        } else {
            BoundBinaryExpression(left, op, right)
        }
    }

    private fun bindLiteralExpression(syntax: LiteralExpressionSyntax): BoundExpression {
        return BoundLiteralExpression(syntax.value ?: 0)
    }

    private fun bindNameExpression(syntax: NameExpressionSyntax): BoundExpression {
        val name = syntax.identifierToken.text
        val variable = _scope.tryLookup(name)
        return if (variable == null) {
            _diagnostics.reportUndefinedName(syntax.identifierToken.span, name)
            BoundLiteralExpression(0)
        } else {
            BoundVariableExpression(variable)
        }
    }

    private fun bindParenthesizedExpression(syntax: ParenthesizedExpressionSyntax): BoundExpression {
        return bindExpression(syntax.expression)
    }

    private fun bindUnaryExpression(syntax: UnaryExpressionSyntax): BoundExpression {
        val operand = bindExpression(syntax.operand)

        val op = BoundUnaryOperator.bind(syntax.operatorToken.kind, operand.type)
        return if (op == null) {
            _diagnostics.reportUndefinedUnaryOperator(
                syntax.operatorToken.span,
                syntax.operatorToken.text,
                operand.type,
            )
            operand
        } else {
            BoundUnaryExpression(op, operand)
        }
    }
}
