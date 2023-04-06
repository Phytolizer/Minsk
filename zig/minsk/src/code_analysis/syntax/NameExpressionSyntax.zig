const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");
const Object = @import("minsk_runtime").Object;

base: ExpressionSyntax,
identifier_token: SyntaxToken,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    identifier_token: SyntaxToken,
) !*ExpressionSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = ExpressionSyntax.init(.name_expression, &deinit, &children),
        .identifier_token = identifier_token,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = ExpressionSyntax.downcast(node, Self);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) std.mem.Allocator.Error![]*const SyntaxNode {
    const self = ExpressionSyntax.downcast(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{&self.identifier_token.base});
}
