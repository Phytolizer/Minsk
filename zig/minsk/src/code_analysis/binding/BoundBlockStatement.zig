const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundStatement = @import("BoundStatement.zig");

base: BoundStatement,
statements: []const *BoundStatement,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    statements: []const *BoundStatement,
) !*BoundStatement {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundStatement.init(.block_statement, &deinit, &children),
        .statements = statements,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundStatement.downcastNode(node, Self);
    for (self.statements) |stmt|
        stmt.deinit(allocator);

    allocator.free(self.statements);
    allocator.destroy(self);
}

fn children(node: *const BoundNode, allocator: std.mem.Allocator) ![]*const BoundNode {
    const self = BoundStatement.downcastNode(node, Self);
    const result = try allocator.alloc(*const BoundNode, self.statements.len);
    for (self.statements, result) |s, *r| {
        r.* = &s.base;
    }
    return result;
}
