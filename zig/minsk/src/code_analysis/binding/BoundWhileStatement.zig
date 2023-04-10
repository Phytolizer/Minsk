const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundStatement = @import("BoundStatement.zig");
const BoundExpression = @import("BoundExpression.zig");

base: BoundStatement,
condition: *BoundExpression,
body: *BoundStatement,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    condition: *BoundExpression,
    body: *BoundStatement,
) !*BoundStatement {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundStatement.init(.while_statement, &deinit),
        .condition = condition,
        .body = body,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundStatement.downcastNode(node, Self);
    self.condition.deinit(allocator);
    self.body.deinit(allocator);
    allocator.destroy(self);
}
