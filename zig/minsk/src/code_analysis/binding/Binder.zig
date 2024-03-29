const std = @import("std");
const CompilationUnitSyntax = @import("../syntax/CompilationUnitSyntax.zig");

const ExpressionSyntax = @import("../syntax/ExpressionSyntax.zig");
const AssignmentExpressionSyntax = @import("../syntax/AssignmentExpressionSyntax.zig");
const BinaryExpressionSyntax = @import("../syntax/BinaryExpressionSyntax.zig");
const LiteralExpressionSyntax = @import("../syntax/LiteralExpressionSyntax.zig");
const NameExpressionSyntax = @import("../syntax/NameExpressionSyntax.zig");
const ParenthesizedExpressionSyntax = @import("../syntax/ParenthesizedExpressionSyntax.zig");
const UnaryExpressionSyntax = @import("../syntax/UnaryExpressionSyntax.zig");

const StatementSyntax = @import("../syntax/StatementSyntax.zig");
const BlockStatementSyntax = @import("../syntax/BlockStatementSyntax.zig");
const ExpressionStatementSyntax = @import("../syntax/ExpressionStatementSyntax.zig");
const VariableDeclarationSyntax = @import("../syntax/VariableDeclarationSyntax.zig");
const IfStatementSyntax = @import("../syntax/IfStatementSyntax.zig");
const ElseClauseSyntax = @import("../syntax/ElseClauseSyntax.zig");
const WhileStatementSyntax = @import("../syntax/WhileStatementSyntax.zig");
const ForStatementSyntax = @import("../syntax/ForStatementSyntax.zig");

const BoundExpression = @import("BoundExpression.zig");
const BoundScope = @import("BoundScope.zig");
const BoundGlobalScope = @import("BoundGlobalScope.zig");
const BoundAssignmentExpression = @import("BoundAssignmentExpression.zig");
const BoundBinaryExpression = @import("BoundBinaryExpression.zig");
const BoundLiteralExpression = @import("BoundLiteralExpression.zig");
const BoundUnaryExpression = @import("BoundUnaryExpression.zig");
const BoundVariableExpression = @import("BoundVariableExpression.zig");

const BoundStatement = @import("BoundStatement.zig");
const BoundBlockStatement = @import("BoundBlockStatement.zig");
const BoundExpressionStatement = @import("BoundExpressionStatement.zig");
const BoundVariableDeclaration = @import("BoundVariableDeclaration.zig");
const BoundIfStatement = @import("BoundIfStatement.zig");
const BoundWhileStatement = @import("BoundWhileStatement.zig");
const BoundForStatement = @import("BoundForStatement.zig");

const Object = @import("minsk_runtime").Object;

const DiagnosticBag = @import("../DiagnosticBag.zig");
const VariableSymbol = @import("../VariableSymbol.zig");

allocator: std.mem.Allocator,
diagnostics: DiagnosticBag,
scope: *BoundScope,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    parent: ?*BoundScope,
) !Self {
    return .{
        .allocator = allocator,
        .diagnostics = DiagnosticBag.init(allocator),
        .scope = try BoundScope.init(allocator, parent),
    };
}

fn createParentScopes(allocator: std.mem.Allocator, previous: ?*const BoundGlobalScope) !?*BoundScope {
    var prev = previous;
    var stack = std.ArrayList(*const BoundGlobalScope).init(allocator);
    defer stack.deinit();
    while (prev) |p| {
        try stack.append(p);
        prev = p.previous;
    }

    var parent: ?*BoundScope = null;
    while (stack.popOrNull()) |gs| {
        const scope = try BoundScope.init(allocator, parent);
        for (gs.variables) |v| {
            _ = try scope.tryDeclare(v);
        }
        parent = scope;
    }
    return parent;
}

pub fn bindGlobalScope(allocator: std.mem.Allocator, previous: ?*BoundGlobalScope, syntax: *CompilationUnitSyntax) !*BoundGlobalScope {
    const parent_scope = try createParentScopes(allocator, previous);
    var binder = try init(allocator, parent_scope);
    defer binder.deinit();
    const statement = try binder.bindStatement(syntax.statement);
    errdefer statement.deinit(allocator);
    const variables = try binder.scope.getDeclaredVariables();
    errdefer {
        for (variables) |v| {
            v.deinit(allocator);
        }
        allocator.free(variables);
    }
    var diagnostics = DiagnosticBag.init(allocator);
    std.mem.swap(DiagnosticBag, &diagnostics, &binder.diagnostics);
    return try BoundGlobalScope.init(
        allocator,
        null,
        diagnostics,
        variables,
        statement,
    );
}

pub fn deinit(self: Self) void {
    self.scope.deinit(.with_parents);
    self.diagnostics.deinit();
}

const AllocError = std.mem.Allocator.Error;

fn bindStatement(self: *Self, syntax: *StatementSyntax) AllocError!*BoundStatement {
    return switch (syntax.base.kind) {
        .expression_statement => try self.bindExpressionStatement(
            StatementSyntax.downcast(syntax, ExpressionStatementSyntax),
        ),
        .block_statement => try self.bindBlockStatement(
            StatementSyntax.downcast(syntax, BlockStatementSyntax),
        ),
        .for_statement => try self.bindForStatement(
            StatementSyntax.downcast(syntax, ForStatementSyntax),
        ),
        .if_statement => try self.bindIfStatement(
            StatementSyntax.downcast(syntax, IfStatementSyntax),
        ),
        .while_statement => try self.bindWhileStatement(
            StatementSyntax.downcast(syntax, WhileStatementSyntax),
        ),
        .variable_declaration => try self.bindVariableDeclaration(
            StatementSyntax.downcast(syntax, VariableDeclarationSyntax),
        ),
        else => unreachable,
    };
}

fn bindBlockStatement(self: *Self, syntax: *BlockStatementSyntax) !*BoundStatement {
    var statements = std.ArrayList(*BoundStatement).init(self.allocator);
    try statements.resize(syntax.statements.len);

    const scope = try BoundScope.init(self.allocator, self.scope);
    defer scope.deinit(.without_parents);
    const old_scope = self.scope;
    self.scope = scope;
    defer self.scope = old_scope;

    for (syntax.statements, statements.items) |stmt, *out| {
        out.* = try self.bindStatement(stmt);
    }

    return try BoundBlockStatement.init(self.allocator, try statements.toOwnedSlice());
}

fn bindIfStatement(self: *Self, syntax: *IfStatementSyntax) !*BoundStatement {
    const condition = try self.bindExpression(syntax.condition, .{ .ty = .boolean });
    const then_statement = try self.bindStatement(syntax.then_statement);
    const else_statement = if (syntax.else_clause) |ec|
        try self.bindStatement(ec.else_statement)
    else
        null;
    return try BoundIfStatement.init(self.allocator, condition, then_statement, else_statement);
}

fn bindForStatement(self: *Self, syntax: *ForStatementSyntax) !*BoundStatement {
    const lower_bound = try self.bindExpression(syntax.lower_bound, .{ .ty = .integer });
    const upper_bound = try self.bindExpression(syntax.upper_bound, .{ .ty = .integer });

    const scope = try BoundScope.init(self.allocator, self.scope);
    defer scope.deinit(.without_parents);
    const old_scope = self.scope;
    self.scope = scope;
    defer self.scope = old_scope;

    const name = syntax.identifier_token.text;
    const variable = VariableSymbol{
        .name = name,
        .ty = .integer,
        .is_read_only = true,
    };
    // new scope, this should never fail
    std.debug.assert(try self.scope.tryDeclare(variable));

    const body = try self.bindStatement(syntax.body);

    return try BoundForStatement.init(
        self.allocator,
        variable,
        lower_bound,
        upper_bound,
        body,
    );
}

fn bindWhileStatement(self: *Self, syntax: *WhileStatementSyntax) !*BoundStatement {
    const condition = try self.bindExpression(syntax.condition, .{ .ty = .boolean });
    const body = try self.bindStatement(syntax.body);
    return try BoundWhileStatement.init(self.allocator, condition, body);
}

fn bindExpressionStatement(self: *Self, syntax: *ExpressionStatementSyntax) !*BoundStatement {
    const expression = try self.bindExpression(syntax.expression, .{});
    return try BoundExpressionStatement.init(self.allocator, expression);
}

fn bindVariableDeclaration(self: *Self, syntax: *VariableDeclarationSyntax) !*BoundStatement {
    const name = syntax.identifier_token.text;
    const initializer = try self.bindExpression(syntax.initializer, .{});
    const is_read_only = syntax.keyword_token.kind == .let_keyword;
    const variable = VariableSymbol{
        .name = name,
        .ty = initializer.type(),
        .is_read_only = is_read_only,
    };

    if (!try self.scope.tryDeclare(variable)) {
        try self.diagnostics.reportVariableAlreadyDeclared(syntax.identifier_token.span(), name);
    }
    return try BoundVariableDeclaration.init(self.allocator, variable, initializer);
}

fn bindExpression(
    self: *Self,
    syntax: *ExpressionSyntax,
    comptime opts: struct { ty: ?Object.Type = null },
) AllocError!*BoundExpression {
    const result = switch (syntax.base.kind) {
        .assignment_expression => try self.bindAssignmentExpression(
            ExpressionSyntax.downcast(&syntax.base, AssignmentExpressionSyntax),
        ),
        .binary_expression => try self.bindBinaryExpression(
            ExpressionSyntax.downcast(&syntax.base, BinaryExpressionSyntax),
        ),
        .literal_expression => try self.bindLiteralExpression(
            ExpressionSyntax.downcast(&syntax.base, LiteralExpressionSyntax),
        ),
        .name_expression => try self.bindNameExpression(
            ExpressionSyntax.downcast(&syntax.base, NameExpressionSyntax),
        ),
        .parenthesized_expression => try self.bindParenthesizedExpression(
            ExpressionSyntax.downcast(&syntax.base, ParenthesizedExpressionSyntax),
        ),
        .unary_expression => try self.bindUnaryExpression(
            ExpressionSyntax.downcast(&syntax.base, UnaryExpressionSyntax),
        ),
        else => unreachable,
    };
    if (opts.ty) |ty| if (result.type() != ty) {
        try self.diagnostics.reportCannotConvert(
            try syntax.span(self.allocator),
            result.type(),
            ty,
        );
    };

    return result;
}

fn bindAssignmentExpression(self: *Self, syntax: *AssignmentExpressionSyntax) !*BoundExpression {
    const name = syntax.identifier_token.text;
    const expression = try self.bindExpression(syntax.expression, .{});
    const variable = self.scope.tryLookup(name) orelse {
        try self.diagnostics.reportUndefinedName(syntax.identifier_token.span(), name);
        return expression;
    };

    if (variable.is_read_only) {
        try self.diagnostics.reportCannotAssign(syntax.equals_token.span(), name);
    }

    if (expression.type() != variable.ty) {
        try self.diagnostics.reportCannotConvert(
            try syntax.expression.span(self.allocator),
            expression.type(),
            variable.ty,
        );
        return expression;
    }

    return try BoundAssignmentExpression.init(self.allocator, variable, expression);
}

fn bindBinaryExpression(self: *Self, syntax: *BinaryExpressionSyntax) !*BoundExpression {
    const left = try self.bindExpression(syntax.left, .{});
    const right = try self.bindExpression(syntax.right, .{});
    const operator = BoundBinaryExpression.Operator.bind(
        syntax.operator_token.kind,
        left.type(),
        right.type(),
    ) orelse {
        try self.diagnostics.reportUndefinedBinaryOperator(
            syntax.operator_token.span(),
            syntax.operator_token.text,
            left.type(),
            right.type(),
        );
        right.deinit(self.allocator);
        return left;
    };
    return try BoundBinaryExpression.init(self.allocator, left, operator, right);
}

fn bindLiteralExpression(self: *Self, syntax: *LiteralExpressionSyntax) !*BoundExpression {
    const value = syntax.value;
    return try BoundLiteralExpression.init(self.allocator, value);
}

fn bindNameExpression(self: *Self, syntax: *NameExpressionSyntax) !*BoundExpression {
    const name = syntax.identifier_token.text;
    if (name.len == 0) {
        // inserted token, don't report any error here
        return try BoundLiteralExpression.init(self.allocator, .{ .integer = 0 });
    }
    const variable = self.scope.tryLookup(name) orelse {
        try self.diagnostics.reportUndefinedName(syntax.identifier_token.span(), name);
        return try BoundLiteralExpression.init(self.allocator, .{ .integer = 0 });
    };

    return try BoundVariableExpression.init(self.allocator, variable);
}

fn bindParenthesizedExpression(self: *Self, syntax: *ParenthesizedExpressionSyntax) !*BoundExpression {
    return try self.bindExpression(syntax.expression, .{});
}

fn bindUnaryExpression(self: *Self, syntax: *UnaryExpressionSyntax) !*BoundExpression {
    const operand = try self.bindExpression(syntax.operand, .{});
    const operator = BoundUnaryExpression.Operator.bind(
        syntax.operator_token.kind,
        operand.type(),
    ) orelse {
        try self.diagnostics.reportUndefinedUnaryOperator(
            syntax.operator_token.span(),
            syntax.operator_token.text,
            operand.type(),
        );
        return operand;
    };
    return try BoundUnaryExpression.init(self.allocator, operator, operand);
}
