const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const StatementSyntax = @import("StatementSyntax.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

const AllocError = std.mem.Allocator.Error;

base: StatementSyntax,
keyword_token: SyntaxToken,
condition: *ExpressionSyntax,
body: *StatementSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    keyword_token: SyntaxToken,
    condition: *ExpressionSyntax,
    body: *StatementSyntax,
) !*StatementSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = StatementSyntax.init(.while_statement, &deinit, &children, null),
        .keyword_token = keyword_token,
        .condition = condition,
        .body = body,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = StatementSyntax.downcastNode(node, Self);
    self.condition.deinit(allocator);
    self.body.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) AllocError![]*const SyntaxNode {
    const self = StatementSyntax.downcastNode(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{
        &self.keyword_token.base,
        &self.condition.base,
        &self.body.base,
    });
}
