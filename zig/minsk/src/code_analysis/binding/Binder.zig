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

allocator: std.mem.Allocator,
diagnostics: std.ArrayList([]const u8),

const Self = @This();

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .diagnostics = std.ArrayList([]const u8).init(allocator),
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
    const operator_kind = BoundBinaryExpression.OperatorKind.bind(syntax.operator_token.kind, left.type(), right.type()) orelse {
        try self.diagnostics.append(try std.fmt.allocPrint(
            self.allocator,
            "Binary operator '{s}' is not defined for types {s} and {s}.",
            .{ syntax.operator_token.text, std.meta.tagName(left.type()), std.meta.tagName(right.type()) },
        ));
        return left;
    };
    return try BoundBinaryExpression.init(self.allocator, left, operator_kind, right);
}

fn bindLiteralExpression(self: *Self, syntax: *LiteralExpressionSyntax) !*BoundExpression {
    const value = syntax.literal_token.value orelse Object{ .int = 0 };
    return try BoundLiteralExpression.init(self.allocator, value);
}

fn bindParenthesizedExpression(self: *Self, syntax: *ParenthesizedExpressionSyntax) !*BoundExpression {
    return try self.bindExpression(syntax.expression);
}

fn bindUnaryExpression(self: *Self, syntax: *UnaryExpressionSyntax) !*BoundExpression {
    const operand = try self.bindExpression(syntax.operand);
    const operator_kind = BoundUnaryExpression.OperatorKind.bind(syntax.operator_token.kind, operand.type()) orelse {
        try self.diagnostics.append(try std.fmt.allocPrint(
            self.allocator,
            "Unary operator '{s}' is not defined for type {s}.",
            .{ syntax.operator_token.text, std.meta.tagName(operand.type()) },
        ));
        return operand;
    };
    return try BoundUnaryExpression.init(self.allocator, operator_kind, operand);
}
