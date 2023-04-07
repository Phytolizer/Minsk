const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

base: ExpressionSyntax,
open_parenthesis_token: SyntaxToken,
expression: *ExpressionSyntax,
close_parenthesis_token: SyntaxToken,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    open_parenthesis_token: SyntaxToken,
    expression: *ExpressionSyntax,
    close_parenthesis_token: SyntaxToken,
) !*ExpressionSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = ExpressionSyntax.init(.parenthesized_expression, &deinit, &children, null),
        .open_parenthesis_token = open_parenthesis_token,
        .expression = expression,
        .close_parenthesis_token = close_parenthesis_token,
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
        &self.open_parenthesis_token.base,
        &self.expression.base,
        &self.close_parenthesis_token.base,
    });
}
