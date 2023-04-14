const std = @import("std");

pub fn build(b: *std.Build, target: std.zig.CrossTarget, optimize: std.builtin.Mode) struct {
    raw: *std.Build.CompileStep,
    lib: *std.Build.InstallArtifactStep,
    inc: *std.Build.InstallFileStep,
} {
    const this_dir = comptime std.fs.path.dirname(@src().file) orelse ".";
    const result = b.addStaticLibrary(.{
        .name = "linenoise",
        .target = target,
        .optimize = optimize,
    });
    result.linkLibC();
    result.addCSourceFile(this_dir ++ "/linenoise-ship.c", &.{"-DUSE_UTF8"});
    result.addIncludePath(this_dir);
    const lib = b.addInstallArtifact(result);
    const inc = b.addInstallHeaderFile(this_dir ++ "/linenoise.h", "linenoise.h");
    return .{ .raw = result, .lib = lib, .inc = inc };
}
