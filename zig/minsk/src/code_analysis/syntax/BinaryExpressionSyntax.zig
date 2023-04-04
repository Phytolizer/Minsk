const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

base: ExpressionSyntax,
left: *ExpressionSyntax,
operator_token: SyntaxToken,
right: *ExpressionSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    left: *ExpressionSyntax,
    operator_token: SyntaxToken,
    right: *ExpressionSyntax,
) !*ExpressionSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = ExpressionSyntax.init(.binary_expression, &deinit, &children),
        .left = left,
        .operator_token = operator_token,
        .right = right,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = ExpressionSyntax.downcast(node, Self);
    self.left.deinit(allocator);
    self.right.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const *const SyntaxNode {
    const self = ExpressionSyntax.downcast(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{ &self.left.base, &self.operator_token.base, &self.right.base });
}
