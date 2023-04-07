const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

base: ExpressionSyntax,
identifier_token: SyntaxToken,
equals_token: SyntaxToken,
expression: *ExpressionSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    identifier_token: SyntaxToken,
    equals_token: SyntaxToken,
    expression: *ExpressionSyntax,
) !*ExpressionSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = ExpressionSyntax.init(.assignment_expression, &deinit, &children, null),
        .identifier_token = identifier_token,
        .equals_token = equals_token,
        .expression = expression,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = ExpressionSyntax.downcast(node, Self);
    self.expression.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) std.mem.Allocator.Error![]*const SyntaxNode {
    const self = ExpressionSyntax.downcast(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{
        &self.identifier_token.base,
        &self.equals_token.base,
        &self.expression.base,
    });
}
