const std = @import("std");
const ExpressionSyntax = @import("../syntax/ExpressionSyntax.zig");
const BinaryExpressionSyntax = @import("../syntax/BinaryExpressionSyntax.zig");
const LiteralExpressionSyntax = @import("../syntax/LiteralExpressionSyntax.zig");
const ParenthesizedExpressionSyntax = @import("../syntax/ParenthesizedExpressionSyntax.zig");
const UnaryExpressionSyntax = @import("../syntax/UnaryExpressionSyntax.zig");

const BoundExpression = @import("BoundExpression.zig");
const BoundBinaryExpression = @import("BoundBinaryExpression.zig");
const BoundLiteralExpression = @import("BoundLiteralExpression.zig");
const BoundUnaryExpression = @import("BoundUnaryExpression.zig");

const Object = @import("minsk_runtime").Object;

const DiagnosticBag = @import("../DiagnosticBag.zig");

allocator: std.mem.Allocator,
diagnostics: DiagnosticBag,

const Self = @This();

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .diagnostics = DiagnosticBag.init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.diagnostics.deinit();
}

pub fn bindExpression(self: *Self, syntax: *ExpressionSyntax) std.mem.Allocator.Error!*BoundExpression {
    return switch (syntax.base.kind) {
        .binary_expression => try self.bindBinaryExpression(ExpressionSyntax.downcast(&syntax.base, BinaryExpressionSyntax)),
        .literal_expression => try self.bindLiteralExpression(ExpressionSyntax.downcast(&syntax.base, LiteralExpressionSyntax)),
        .parenthesized_expression => try self.bindParenthesizedExpression(ExpressionSyntax.downcast(&syntax.base, ParenthesizedExpressionSyntax)),
        .unary_expression => try self.bindUnaryExpression(ExpressionSyntax.downcast(&syntax.base, UnaryExpressionSyntax)),
        else => unreachable,
    };
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
