const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const StatementSyntax = @import("StatementSyntax.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

const AllocError = std.mem.Allocator.Error;

base: StatementSyntax,
keyword_token: SyntaxToken,
identifier_token: SyntaxToken,
equals_token: SyntaxToken,
initializer: *ExpressionSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    keyword_token: SyntaxToken,
    identifier_token: SyntaxToken,
    equals_token: SyntaxToken,
    initializer: *ExpressionSyntax,
) !*StatementSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = StatementSyntax.init(.variable_declaration, &deinit, &children, null),
        .keyword_token = keyword_token,
        .identifier_token = identifier_token,
        .equals_token = equals_token,
        .initializer = initializer,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = StatementSyntax.downcastNode(node, Self);
    self.initializer.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) AllocError![]*const SyntaxNode {
    const self = StatementSyntax.downcastNode(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{
        &self.keyword_token.base,
        &self.identifier_token.base,
        &self.equals_token.base,
        &self.initializer.base,
    });
}
