const std = @import("std");
const snakeToCamel = @import("minsk_meta").snakeToCamel;

pub const Object = union(Type) {
    integer: i64,
    boolean: bool,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .integer => |v| try writer.print("{d}", .{v}),
            .boolean => |v| try writer.print("{s}", .{if (v) "true" else "false"}),
        }
    }

    pub fn tag(self: Self) Type {
        return std.meta.activeTag(self);
    }

    pub fn eq(self: Self, other: Self) bool {
        return switch (self) {
            .boolean => |s| switch (other) {
                .boolean => |o| s == o,
                else => false,
            },
            .integer => |s| switch (other) {
                .integer => |o| s == o,
                else => false,
            },
        };
    }

    pub const Type = enum {
        integer,
        boolean,

        const display_names = blk: {
            const fields = std.meta.fields(Type);
            var result: [fields.len][]const u8 = undefined;
            for (&result, 0..) |*r, i| {
                r.* = snakeToCamel(fields[i].name);
            }
            break :blk result;
        };

        pub fn displayName(self: Type) []const u8 {
            return display_names[@enumToInt(self)];
        }
    };
};
