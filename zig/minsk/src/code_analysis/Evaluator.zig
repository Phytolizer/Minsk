const std = @import("std");
const ExpressionSyntax = @import("syntax/ExpressionSyntax.zig");
const LiteralExpressionSyntax = @import("syntax/LiteralExpressionSyntax.zig");
const BinaryExpressionSyntax = @import("syntax/BinaryExpressionSyntax.zig");
const ParenthesizedExpressionSyntax = @import("syntax/ParenthesizedExpressionSyntax.zig");
const UnaryExpressionSyntax = @import("syntax/UnaryExpressionSyntax.zig");

root: *const ExpressionSyntax,

const Self = @This();

pub fn init(root: *const ExpressionSyntax) Self {
    return .{
        .root = root,
    };
}

pub fn evaluate(self: Self) i64 {
    return self.evaluateExpression(self.root);
}

fn evaluateExpression(self: Self, node: *const ExpressionSyntax) i64 {
    switch (node.base.kind) {
        .literal_expression => {
            return ExpressionSyntax.downcast(&node.base, LiteralExpressionSyntax).literal_token.value.?.int;
        },
        .binary_expression => {
            const b = ExpressionSyntax.downcast(&node.base, BinaryExpressionSyntax);
            const left = self.evaluateExpression(b.left);
            const right = self.evaluateExpression(b.right);

            return switch (b.operator_token.kind) {
                .plus_token => left + right,
                .minus_token => left - right,
                .star_token => @mulWithOverflow(left, right).@"0",
                .slash_token => @divTrunc(left, right),
                else => unreachable,
            };
        },
        .parenthesized_expression => {
            return self.evaluateExpression(
                ExpressionSyntax.downcast(&node.base, ParenthesizedExpressionSyntax).expression,
            );
        },
        .unary_expression => {
            const u = ExpressionSyntax.downcast(&node.base, UnaryExpressionSyntax);
            const operand = self.evaluateExpression(u.operand);

            return switch (u.operator_token.kind) {
                .plus_token => operand,
                .minus_token => -operand,
                else => unreachable,
            };
        },
        else => unreachable,
    }
}
