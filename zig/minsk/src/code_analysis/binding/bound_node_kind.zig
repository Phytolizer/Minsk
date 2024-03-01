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
        for (&result, fields) |*r, field| {
            r.* = snakeToCamel(field.name);
        }
        break :blk result;
    };

    pub fn displayName(self: Self) []const u8 {
        return display_names[@intFromEnum(self)];
    }

    fn suffixMap(comptime fields: []const std.builtin.Type.EnumField, comptime suffixes: []const []const u8) [fields.len]bool {
        var result: [fields.len]bool = [_]bool{false} ** fields.len;
        for (&result, fields) |*r, field| {
            inline for (suffixes) |suffix| {
                if (std.mem.endsWith(u8, field.name, suffix)) {
                    r.* = true;
                }
            }
        }
        return result;
    }

    pub fn isExpression(self: Self) bool {
        const map = comptime suffixMap(std.meta.fields(Self), &.{"_expression"});
        return map[@intFromEnum(self)];
    }

    pub fn isStatement(self: Self) bool {
        const map = comptime suffixMap(std.meta.fields(Self), &.{"_statement, _declaration"});
        return map[@intFromEnum(self)];
    }
};
