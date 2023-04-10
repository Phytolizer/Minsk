const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const StatementSyntax = @import("StatementSyntax.zig");
const ExpressionSyntax = @import("ExpressionSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");
const ElseClauseSyntax = @import("ElseClauseSyntax.zig");

const AllocError = std.mem.Allocator.Error;

base: StatementSyntax,
keyword_token: SyntaxToken,
condition: *ExpressionSyntax,
then_statement: *StatementSyntax,
else_clause: ?*ElseClauseSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    keyword_token: SyntaxToken,
    condition: *ExpressionSyntax,
    then_statement: *StatementSyntax,
    else_clause: ?*ElseClauseSyntax,
) !*StatementSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = StatementSyntax.init(.if_statement, &deinit, &children, null),
        .keyword_token = keyword_token,
        .condition = condition,
        .then_statement = then_statement,
        .else_clause = else_clause,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = StatementSyntax.downcastNode(node, Self);
    self.condition.deinit(allocator);
    self.then_statement.deinit(allocator);
    if (self.else_clause) |ec| ec.base.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) AllocError![]*const SyntaxNode {
    const self = StatementSyntax.downcastNode(node, Self);
    var result = std.ArrayList(*const SyntaxNode).init(allocator);
    try result.appendSlice(&.{
        &self.keyword_token.base,
        &self.condition.base,
        &self.then_statement.base,
    });
    if (self.else_clause) |ec| try result.append(&ec.base);
    return try result.toOwnedSlice();
}
