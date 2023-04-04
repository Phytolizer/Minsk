const std = @import("std");
const BoundExpression = @import("binding/BoundExpression.zig");
const BoundBinaryExpression = @import("binding/BoundBinaryExpression.zig");
const BoundLiteralExpression = @import("binding/BoundLiteralExpression.zig");
const BoundUnaryExpression = @import("binding/BoundUnaryExpression.zig");
const Object = @import("minsk_runtime").Object;

root: *const BoundExpression,

const Self = @This();

pub fn init(root: *const BoundExpression) Self {
    return .{
        .root = root,
    };
}

pub fn evaluate(self: Self) Object {
    return self.evaluateExpression(self.root);
}

fn evaluateExpression(self: Self, node: *const BoundExpression) Object {
    return switch (node.base.kind) {
        .binary_expression => self.evaluateBinaryExpression(
            BoundExpression.downcast(node, BoundBinaryExpression),
        ),
        .literal_expression => self.evaluateLiteralExpression(
            BoundExpression.downcast(node, BoundLiteralExpression),
        ),
        .unary_expression => self.evaluateUnaryExpression(
            BoundExpression.downcast(node, BoundUnaryExpression),
        ),
    };
}

fn evaluateBinaryExpression(self: Self, node: *const BoundBinaryExpression) Object {
    const left = self.evaluateExpression(node.left);
    const right = self.evaluateExpression(node.right);

    return switch (node.operator.kind) {
        .addition => .{ .integer = left.integer + right.integer },
        .subtraction => .{ .integer = left.integer - right.integer },
        .multiplication => .{ .integer = @mulWithOverflow(left.integer, right.integer).@"0" },
        .division => .{ .integer = @divTrunc(left.integer, right.integer) },
        .logical_and => .{ .boolean = left.boolean and right.boolean },
        .logical_or => .{ .boolean = left.boolean or right.boolean },
    };
}

fn evaluateLiteralExpression(_: Self, node: *const BoundLiteralExpression) Object {
    return node.value;
}

fn evaluateUnaryExpression(self: Self, node: *const BoundUnaryExpression) Object {
    const operand = self.evaluateExpression(node.operand);

    return switch (node.operator.kind) {
        .identity => .{ .integer = operand.integer },
        .negation => .{ .integer = -operand.integer },
        .logical_negation => .{ .boolean = !operand.boolean },
    };
}
