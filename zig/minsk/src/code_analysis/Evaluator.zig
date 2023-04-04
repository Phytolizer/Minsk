const std = @import("std");
const BoundExpression = @import("binding/BoundExpression.zig");
const BoundBinaryExpression = @import("binding/BoundBinaryExpression.zig");
const BoundLiteralExpression = @import("binding/BoundLiteralExpression.zig");
const BoundUnaryExpression = @import("binding/BoundUnaryExpression.zig");

root: *const BoundExpression,

const Self = @This();

pub fn init(root: *const BoundExpression) Self {
    return .{
        .root = root,
    };
}

pub fn evaluate(self: Self) i64 {
    return self.evaluateExpression(self.root);
}

fn evaluateExpression(self: Self, node: *const BoundExpression) i64 {
    switch (node.base.kind) {
        .literal_expression => {
            return BoundExpression.downcast(node, BoundLiteralExpression).value.int;
        },
        .binary_expression => {
            const b = BoundExpression.downcast(node, BoundBinaryExpression);
            const left = self.evaluateExpression(b.left);
            const right = self.evaluateExpression(b.right);

            return switch (b.operator_kind) {
                .addition => left + right,
                .subtraction => left - right,
                .multiplication => @mulWithOverflow(left, right).@"0",
                .division => @divTrunc(left, right),
            };
        },
        .unary_expression => {
            const u = BoundExpression.downcast(node, BoundUnaryExpression);
            const operand = self.evaluateExpression(u.operand);

            return switch (u.operator_kind) {
                .identity => operand,
                .negation => -operand,
            };
        },
    }
}
