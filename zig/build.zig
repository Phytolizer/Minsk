const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const minsk_mod = @import("minsk/build.zig").build(b);
    const ds_ext_mod = @import("ds_ext/build.zig").build(b);
    const minsk_runtime_mod = @import("minsk_runtime/build.zig").build(b);

    minsk_mod.dependencies.put("ds_ext", ds_ext_mod) catch unreachable;
    minsk_mod.dependencies.put("minsk_runtime", minsk_runtime_mod) catch unreachable;

    const ziglyph_dep = b.dependency("ziglyph", .{
        .target = target,
        .optimize = optimize,
    });
    minsk_mod.dependencies.put(
        "ziglyph",
        ziglyph_dep.module("ziglyph"),
    ) catch unreachable;
    const mc_exe = @import("mc/build.zig").build(b, target, optimize);

    mc_exe.addModule("minsk", minsk_mod);
    mc_exe.install();

    const mc_run = b.addRunArtifact(mc_exe);
    if (b.args) |args| {
        mc_run.addArgs(args);
    }
    const run_step = b.step("run", "Run mc");
    run_step.dependOn(&mc_run.step);
}
