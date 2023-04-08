const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const StatementSyntax = @import("StatementSyntax.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

const AllocError = std.mem.Allocator.Error;

base: StatementSyntax,
expression: *ExpressionSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    expression: *ExpressionSyntax,
) !*StatementSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = StatementSyntax.init(.expression_statement, &deinit, &children, null),
        .expression = expression,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = StatementSyntax.downcastNode(node, Self);
    self.expression.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) AllocError![]*const SyntaxNode {
    const self = StatementSyntax.downcastNode(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{&self.expression.base});
}
