const std = @import("std");

pub fn build(b: *std.Build, target: std.zig.CrossTarget, optimize: std.builtin.Mode) *std.Build.CompileStep {
    const this_dir = comptime std.fs.path.dirname(@src().file) orelse ".";
    return b.addExecutable(.{
        .name = "mc",
        .root_source_file = .{ .path = this_dir ++ "/src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
}
