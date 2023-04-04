const std = @import("std");

pub fn snakeToCamel(comptime name: []const u8) []const u8 {
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
