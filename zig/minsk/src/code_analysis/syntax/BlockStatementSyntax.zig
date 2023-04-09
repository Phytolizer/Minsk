const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const StatementSyntax = @import("StatementSyntax.zig");
const SyntaxToken = @import("SyntaxToken.zig");

const AllocError = std.mem.Allocator.Error;

base: StatementSyntax,
open_brace_token: SyntaxToken,
statements: []const *StatementSyntax,
close_brace_token: SyntaxToken,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    open_brace_token: SyntaxToken,
    statements: []const *StatementSyntax,
    close_brace_token: SyntaxToken,
) !*StatementSyntax {
    const result = try allocator.create(Self);
    result.* = .{
        .base = StatementSyntax.init(.block_statement, &deinit, &children, null),
        .open_brace_token = open_brace_token,
        .statements = statements,
        .close_brace_token = close_brace_token,
    };
    return &result.base;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = StatementSyntax.downcastNode(node, Self);
    for (self.statements) |stmt| {
        stmt.deinit(allocator);
    }
    allocator.free(self.statements);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) AllocError![]*const SyntaxNode {
    const self = StatementSyntax.downcastNode(node, Self);
    var result = try std.ArrayList(*const SyntaxNode).initCapacity(allocator, self.statements.len + 2);
    defer result.deinit();
    try result.append(&self.open_brace_token.base);
    for (self.statements) |stmt| {
        try result.append(&stmt.base);
    }
    try result.append(&self.close_brace_token.base);
    return try result.toOwnedSlice();
}
