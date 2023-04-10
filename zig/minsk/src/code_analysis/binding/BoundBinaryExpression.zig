const std = @import("std");
const BoundNode = @import("BoundNode.zig");
const BoundExpression = @import("BoundExpression.zig");
const Object = @import("minsk_runtime").Object;
const SyntaxKind = @import("../syntax/syntax_kind.zig").SyntaxKind;

base: BoundExpression,
left: *BoundExpression,
operator: Operator,
right: *BoundExpression,

pub const Operator = struct {
    syntax_kind: SyntaxKind,
    kind: Kind,
    left_type: Object.Type,
    right_type: Object.Type,
    result_type: Object.Type,

    pub const Kind = enum {
        addition,
        subtraction,
        multiplication,
        division,
        logical_and,
        logical_or,
        equality,
        inequality,
        less_than,
        less_than_or_equal,
        greater_than,
        greater_than_or_equal,
        bitwise_and,
        bitwise_or,
        bitwise_xor,
    };

    fn init(
        syntax_kind: SyntaxKind,
        kind: Kind,
        left_type: Object.Type,
        right_type: Object.Type,
        result_type: Object.Type,
    ) Operator {
        return .{
            .syntax_kind = syntax_kind,
            .kind = kind,
            .left_type = left_type,
            .right_type = right_type,
            .result_type = result_type,
        };
    }

    fn initMatching(syntax_kind: SyntaxKind, kind: Kind, matching_type: Object.Type) Operator {
        return Operator.init(
            syntax_kind,
            kind,
            matching_type,
            matching_type,
            matching_type,
        );
    }

    const operators = [_]Operator{
        initMatching(.plus_token, .addition, .integer),
        initMatching(.minus_token, .subtraction, .integer),
        initMatching(.star_token, .multiplication, .integer),
        initMatching(.slash_token, .division, .integer),
        initMatching(.ampersand_ampersand_token, .logical_and, .boolean),
        initMatching(.pipe_pipe_token, .logical_or, .boolean),

        initMatching(.equals_equals_token, .equality, .boolean),
        Operator.init(.equals_equals_token, .equality, .integer, .integer, .boolean),

        Operator.init(.less_token, .less_than, .integer, .integer, .boolean),
        Operator.init(.less_equals_token, .less_than_or_equal, .integer, .integer, .boolean),
        Operator.init(.greater_token, .greater_than, .integer, .integer, .boolean),
        Operator.init(.greater_equals_token, .greater_than_or_equal, .integer, .integer, .boolean),

        initMatching(.bang_equals_token, .inequality, .boolean),
        Operator.init(.bang_equals_token, .inequality, .integer, .integer, .boolean),

        initMatching(.ampersand_token, .bitwise_and, .integer),
        initMatching(.ampersand_token, .bitwise_and, .boolean),
        initMatching(.pipe_token, .bitwise_or, .integer),
        initMatching(.pipe_token, .bitwise_or, .boolean),
        initMatching(.hat_token, .bitwise_xor, .integer),
        initMatching(.hat_token, .bitwise_xor, .boolean),
    };

    pub fn bind(syntax_kind: SyntaxKind, left_type: Object.Type, right_type: Object.Type) ?Operator {
        for (operators) |op| {
            if (op.syntax_kind == syntax_kind and
                op.left_type == left_type and
                op.right_type == right_type)
            {
                return op;
            }
        }
        return null;
    }
};

const Self = @This();

pub fn init(
    allocator: std.mem.Allocator,
    left: *BoundExpression,
    operator: Operator,
    right: *BoundExpression,
) !*BoundExpression {
    const result = try allocator.create(Self);
    result.* = .{
        .base = BoundExpression.init(.binary_expression, &deinit, &@"type"),
        .left = left,
        .operator = operator,
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
    return self.operator.result_type;
}
