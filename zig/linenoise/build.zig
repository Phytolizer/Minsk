const std = @import("std");

pub fn build(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.Mode) struct {
    raw: *std.Build.Step.Compile,
    lib: *std.Build.Step.InstallArtifact,
} {
    const this_dir = comptime std.fs.path.dirname(@src().file) orelse ".";
    const result = b.addStaticLibrary(.{
        .name = "linenoise",
        .target = target,
        .optimize = optimize,
    });
    result.linkLibC();
    result.addCSourceFile(.{
        .file = .{ .cwd_relative = this_dir ++ "/linenoise-ship.c" },
        .flags = &.{"-DUSE_UTF8"},
    });
    result.addIncludePath(.{ .cwd_relative = this_dir });
    const lib = b.addInstallArtifact(result, .{});
    result.installHeader(.{ .cwd_relative = this_dir ++ "/linenoise.h" }, "linenoise.h");
    return .{ .raw = result, .lib = lib };
}
