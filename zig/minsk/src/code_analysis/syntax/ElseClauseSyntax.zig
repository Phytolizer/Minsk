const std = @import("std");
const SyntaxNode = @import("SyntaxNode.zig");
const SyntaxToken = @import("SyntaxToken.zig");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const TextSpan = @import("../text/TextSpan.zig");
const StatementSyntax = @import("StatementSyntax.zig");

base: SyntaxNode,
keyword_token: SyntaxToken,
else_statement: *StatementSyntax,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    keyword_token: SyntaxToken,
    else_statement: *StatementSyntax,
) !*Self {
    const result = try allocator.create(Self);
    result.* = .{
        .base = .{
            .kind = .else_clause,
            .deinit_fn = &deinit,
            .children_fn = &children,
            .span_fn = null,
        },
        .keyword_token = keyword_token,
        .else_statement = else_statement,
    };
    return result;
}

fn deinit(node: *const SyntaxNode, allocator: std.mem.Allocator) void {
    const self = SyntaxNode.downcast(node, Self);
    self.else_statement.deinit(allocator);
    allocator.destroy(self);
}

fn children(node: *const SyntaxNode, allocator: std.mem.Allocator) ![]*const SyntaxNode {
    const self = SyntaxNode.downcast(node, Self);
    return try allocator.dupe(*const SyntaxNode, &.{
        &self.keyword_token.base,
        &self.else_statement.base,
    });
}
