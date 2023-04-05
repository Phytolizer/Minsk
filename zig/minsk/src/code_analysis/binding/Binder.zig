const std = @import("std");
const ExpressionSyntax = @import("../syntax/ExpressionSyntax.zig");
const AssignmentExpressionSyntax = @import("../syntax/AssignmentExpressionSyntax.zig");
const BinaryExpressionSyntax = @import("../syntax/BinaryExpressionSyntax.zig");
const LiteralExpressionSyntax = @import("../syntax/LiteralExpressionSyntax.zig");
const NameExpressionSyntax = @import("../syntax/NameExpressionSyntax.zig");
const ParenthesizedExpressionSyntax = @import("../syntax/ParenthesizedExpressionSyntax.zig");
const UnaryExpressionSyntax = @import("../syntax/UnaryExpressionSyntax.zig");

const BoundExpression = @import("BoundExpression.zig");
const BoundAssignmentExpression = @import("BoundAssignmentExpression.zig");
const BoundBinaryExpression = @import("BoundBinaryExpression.zig");
const BoundLiteralExpression = @import("BoundLiteralExpression.zig");
const BoundUnaryExpression = @import("BoundUnaryExpression.zig");
const BoundVariableExpression = @import("BoundVariableExpression.zig");

const Object = @import("minsk_runtime").Object;

const DiagnosticBag = @import("../DiagnosticBag.zig");

allocator: std.mem.Allocator,
diagnostics: DiagnosticBag,
variables: *std.StringArrayHashMap(Object),

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    variables: *std.StringArrayHashMap(Object),
) Self {
    return .{
        .allocator = allocator,
        .diagnostics = DiagnosticBag.init(allocator),
        .variables = variables,
    };
}

pub fn deinit(self: Self) void {
    self.diagnostics.deinit();
}

pub fn bindExpression(self: *Self, syntax: *ExpressionSyntax) std.mem.Allocator.Error!*BoundExpression {
    return switch (syntax.base.kind) {
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
}

fn bindAssignmentExpression(self: *Self, syntax: *AssignmentExpressionSyntax) !*BoundExpression {
    const name = syntax.identifier_token.text;
    const expression = try self.bindExpression(syntax.expression);
    return try BoundAssignmentExpression.init(self.allocator, name, expression);
}

fn bindBinaryExpression(self: *Self, syntax: *BinaryExpressionSyntax) !*BoundExpression {
    const left = try self.bindExpression(syntax.left);
    const right = try self.bindExpression(syntax.right);
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
    const value = if (self.variables.get(name)) |value|
        value
    else {
        try self.diagnostics.reportUndefinedName(syntax.identifier_token.span(), name);
        return try BoundLiteralExpression.init(self.allocator, .{ .integer = 0 });
    };
    _ = value;

    const ty = Object.Type.integer;
    return try BoundVariableExpression.init(self.allocator, name, ty);
}

fn bindParenthesizedExpression(self: *Self, syntax: *ParenthesizedExpressionSyntax) !*BoundExpression {
    return try self.bindExpression(syntax.expression);
}

fn bindUnaryExpression(self: *Self, syntax: *UnaryExpressionSyntax) !*BoundExpression {
    const operand = try self.bindExpression(syntax.operand);
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
