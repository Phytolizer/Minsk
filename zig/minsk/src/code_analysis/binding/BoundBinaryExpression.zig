const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundExpression = @import("BoundExpression.zig");
const Object = @import("minsk_runtime").Object;
const SyntaxKind = @import("../syntax/syntax_kind.zig").SyntaxKind;

base: BoundExpression,
left: *BoundExpression,
operator_kind: OperatorKind,
right: *BoundExpression,

pub const OperatorKind = enum {
    addition,
    subtraction,
    multiplication,
    division,

    pub fn bind(syntax_kind: SyntaxKind, left_type: Object.Type, right_type: Object.Type) ?OperatorKind {
        if (left_type != .int or right_type != .int)
            return null;

        return switch (syntax_kind) {
            .plus_token => .addition,
            .minus_token => .subtraction,
            .star_token => .multiplication,
            .slash_token => .division,
            else => unreachable,
        };
    }
};

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    left: *BoundExpression,
    operator_kind: OperatorKind,
    right: *BoundExpression,
) !*BoundExpression {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundExpression.init(.binary_expression, &deinit, &@"type"),
        .left = left,
        .operator_kind = operator_kind,
        .right = right,
    };
    return &result.base;
}

fn deinit(node: *const BoundNode, allocator: std.mem.Allocator) void {
    const self = BoundExpression.downcastNode(node, Self);
    self.left.deinit(allocator);
    self.right.deinit(allocator);
    allocator.destroy(self);
}

fn @"type"(node: *const BoundExpression) Object.Type {
    const self = BoundExpression.downcast(node, Self);
    return self.left.type();
}
