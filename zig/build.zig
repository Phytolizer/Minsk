const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const minsk_mod = @import("minsk/build.zig").build(b);
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
