const std = @import("std");
const snakeToCamel = @import("minsk_meta").snakeToCamel;

pub const BoundNodeKind = enum {
    assignment_expression,
    binary_expression,
    literal_expression,
    unary_expression,
    variable_expression,

    block_statement,
    expression_statement,
    variable_declaration,
    if_statement,
    while_statement,
    for_statement,

    const Self = @This();

    const display_names = blk: {
        const fields = std.meta.fields(Self);
        var result: [fields.len][]const u8 = undefined;
        for (&result, 0..) |*r, i| {
            r.* = snakeToCamel(fields[i].name);
        }
        break :blk result;
    };

    pub fn displayName(self: Self) []const u8 {
        return display_names[@intFromEnum(self)];
    }

    pub fn is_expression(self: Self) bool {
        return switch (self) {
            .assignment_expression,
            .binary_expression,
            .literal_expression,
            .unary_expression,
            .variable_expression,
            => true,
            else => false,
        };
    }

    pub fn is_statement(self: Self) bool {
        return switch (self) {
            .block_statement,
            .expression_statement,
            .variable_declaration,
            .if_statement,
            .while_statement,
            .for_statement,
            => true,
            else => false,
        };
    }
};
