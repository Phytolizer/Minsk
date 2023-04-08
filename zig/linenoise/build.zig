const std = @import("std");

pub fn build(b: *std.Build, target: std.zig.CrossTarget, optimize: std.builtin.Mode) *std.Build.CompileStep {
    const this_dir = comptime std.fs.path.dirname(@src().file) orelse ".";
    const result = b.addStaticLibrary(.{
        .name = "linenoise",
        .target = target,
        .optimize = optimize,
    });
    result.linkLibC();
    result.addCSourceFile(this_dir ++ "/linenoise-ship.c", &.{"-DUSE_UTF8"});
    result.addIncludePath(this_dir);
    result.install();
    result.installHeader(this_dir ++ "/linenoise.h", "linenoise.h");
    return result;
}
