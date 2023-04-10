const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const StatementSyntax = @import("StatementSyntax.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

const AllocError = std.mem.Allocator.Error;

base: StatementSyntax,
for_keyword: SyntaxToken,
identifier_token: SyntaxToken,
equals_token: SyntaxToken,
lower_bound: *ExpressionSyntax,
to_keyword: SyntaxToken,
upper_bound: *ExpressionSyntax,
body: *StatementSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    for_keyword: SyntaxToken,
    identifier_token: SyntaxToken,
    equals_token: SyntaxToken,
    lower_bound: *ExpressionSyntax,
    to_keyword: SyntaxToken,
    upper_bound: *ExpressionSyntax,
    body: *StatementSyntax,
) !*StatementSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = StatementSyntax.init(.for_statement, &deinit, &children, null),
        .for_keyword = for_keyword,
        .identifier_token = identifier_token,
        .equals_token = equals_token,
        .lower_bound = lower_bound,
        .to_keyword = to_keyword,
        .upper_bound = upper_bound,
        .body = body,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = StatementSyntax.downcastNode(node, Self);
    self.lower_bound.deinit(allocator);
    self.upper_bound.deinit(allocator);
    self.body.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) AllocError![]*const SyntaxNode {
    const self = StatementSyntax.downcastNode(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{
        &self.for_keyword.base,
        &self.identifier_token.base,
        &self.equals_token.base,
        &self.lower_bound.base,
        &self.to_keyword.base,
        &self.upper_bound.base,
        &self.body.base,
    });
}
