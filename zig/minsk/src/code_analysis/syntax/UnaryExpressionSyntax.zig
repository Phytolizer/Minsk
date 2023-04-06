const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

base: ExpressionSyntax,
operator_token: SyntaxToken,
operand: *ExpressionSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    operator_token: SyntaxToken,
    operand: *ExpressionSyntax,
) !*ExpressionSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = ExpressionSyntax.init(.unary_expression, &deinit, &children),
        .operator_token = operator_token,
        .operand = operand,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = ExpressionSyntax.downcast(node, Self);
    self.operand.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) std.mem.Allocator.Error![]*const SyntaxNode {
    const self = ExpressionSyntax.downcast(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{
        &self.operator_token.base,
        &self.operand.base,
    });
}
