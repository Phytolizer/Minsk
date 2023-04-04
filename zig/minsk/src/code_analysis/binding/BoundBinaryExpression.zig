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
    logical_and,
    logical_or,

    pub fn bind(syntax_kind: SyntaxKind, left_type: Object.Type, right_type: Object.Type) ?OperatorKind {
        if (left_type == .integer and right_type == .integer) {
            switch (syntax_kind) {
                .plus_token => return .addition,
                .minus_token => return .subtraction,
                .star_token => return .multiplication,
                .slash_token => return .division,
                else => {},
            }
        } else if (left_type == .boolean and right_type == .boolean) {
            switch (syntax_kind) {
                .ampersand_ampersand_token => return .logical_and,
                .pipe_pipe_token => return .logical_or,
                else => {},
            }
        }
        return null;
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
