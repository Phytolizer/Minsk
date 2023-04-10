const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundStatement = @import("BoundStatement.zig");
const BoundExpression = @import("BoundExpression.zig");
const VariableSymbol = @import("../VariableSymbol.zig");

base: BoundStatement,
variable: VariableSymbol,
lower_bound: *BoundExpression,
upper_bound: *BoundExpression,
body: *BoundStatement,

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    variable: VariableSymbol,
    lower_bound: *BoundExpression,
    upper_bound: *BoundExpression,
    body: *BoundStatement,
) !*BoundStatement {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundStatement.init(.for_statement, &deinit),
        .variable = variable,
        .lower_bound = lower_bound,
        .upper_bound = upper_bound,
        .body = body,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundStatement.downcastNode(node, Self);
    self.lower_bound.deinit(allocator);
    self.upper_bound.deinit(allocator);
    self.body.deinit(allocator);
    allocator.destroy(self);
}
