const std = @import("std");

pub fn build(b: *std.Build) *std.Build.Module {
    const this_dir = comptime std.fs.path.dirname(@src().file) orelse ".";
    return b.addModule("minsk", .{
        .root_source_file = .{ .path = this_dir ++ "/src/main.zig" },
    });
}
