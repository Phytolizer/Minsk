const std = @import("std");

pub const SyntaxKind = enum(usize) {
    bad_token,
    end_of_file_token,

    whitespace_token,
    number_token,

    plus_token,
    minus_token,
    star_token,
    slash_token,
    open_parenthesis_token,
    close_parenthesis_token,

    const Self = @This();

    fn snakeToCamel(comptime name: []const u8) []const u8 {
        var result: []const u8 = "";
        var was_underscore = true;
        for (name) |c| {
            if (c == '_') {
                was_underscore = true;
            } else if (was_underscore) {
                was_underscore = false;
                result = result ++ &[_]u8{std.ascii.toUpper(c)};
            } else {
                result = result ++ &[_]u8{c};
            }
        }
        return result;
    }

    const display_names = blk: {
        const fields = std.meta.fields(Self);
        var result: [fields.len][]const u8 = undefined;
        for (&result, 0..) |*r, i| {
            r.* = snakeToCamel(fields[i].name);
        }
        break :blk result;
    };

    pub fn displayName(self: Self) []const u8 {
        return display_names[@enumToInt(self)];
    }
};
