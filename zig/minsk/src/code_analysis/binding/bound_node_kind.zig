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

    fn endsWithOneOf(comptime s: []const u8, comptime suffixes: []const []const u8) bool {
        inline for (suffixes) |suffix| {
            if (std.mem.endsWith(u8, s, suffix)) {
                return true;
            }
        }
        return false;
    }

    fn suffixMap(comptime fields: []const std.builtin.Type.EnumField, comptime suffixes: []const []const u8) [fields.len]bool {
        var result: [fields.len]bool = undefined;
        for (&result, fields) |*r, field| {
            r.* = endsWithOneOf(field.name, suffixes);
        }
        return result;
    }

    pub fn is_expression(self: Self) bool {
        const map = comptime suffixMap(std.meta.fields(Self), &.{"_expression"});
        return map[@intFromEnum(self)];
    }

    pub fn is_statement(self: Self) bool {
        const map = comptime suffixMap(std.meta.fields(Self), &.{"_statement, _declaration"});
        return map[@intFromEnum(self)];
    }
};
