const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

base: ExpressionSyntax,
literal_token: SyntaxToken,

const Self = @This();

pub fn init(allocator: std.mem.Allocator, literal_token: SyntaxToken) !*ExpressionSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = ExpressionSyntax.init(.literal_expression, &deinit, &children),
        .literal_token = literal_token,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = ExpressionSyntax.downcast(node, Self);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const *const SyntaxNode {
    const self = ExpressionSyntax.downcast(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{&self.literal_token.base});
}