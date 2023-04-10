const std = @import("std");

pub fn camelToDisplay(comptime name: []const u8) []const u8 {
    var result: []const u8 = "";
    var cap: ?u8 = null;
    for (name) |c| {
        if (std.ascii.isUpper(c)) {
            cap = std.ascii.toLower(c);
        } else if (cap) |prev| {
            cap = null;
            result = result ++ &[_]u8{ ' ', prev, c };
        } else {
            result = result ++ &[_]u8{c};
        }
    }
    return result;
}
