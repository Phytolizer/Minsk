const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const minsk_mod = @import("minsk/build.zig").build(b);
    const ds_ext_mod = @import("ds_ext/build.zig").build(b);
    const tty_ext_mod = @import("tty_ext/build.zig").build(b);
    const minsk_runtime_mod = @import("minsk_runtime/build.zig").build(b);
    const minsk_meta_mod = @import("minsk_meta/build.zig").build(b);
    const linenoise = @import("linenoise/build.zig").build(b, target, optimize);

    minsk_runtime_mod.addImport("minsk_meta", minsk_meta_mod);

    minsk_mod.addImport("ds_ext", ds_ext_mod);
    minsk_mod.addImport("tty_ext", tty_ext_mod);
    minsk_mod.addImport("minsk_runtime", minsk_runtime_mod);
    minsk_mod.addImport("minsk_meta", minsk_meta_mod);

    const ziglyph_dep = b.dependency("ziglyph", .{
        .target = target,
        .optimize = optimize,
    });
    minsk_mod.addImport(
        "ziglyph",
        ziglyph_dep.module("ziglyph"),
    );
    const mc_exe = @import("mc/build.zig").build(b, target, optimize);

    mc_exe.root_module.addImport("minsk", minsk_mod);
    mc_exe.root_module.addImport("minsk_runtime", minsk_runtime_mod);
    mc_exe.root_module.addImport("tty_ext", tty_ext_mod);
    mc_exe.linkLibrary(linenoise.raw);
    mc_exe.step.dependOn(&linenoise.lib.step);
    b.installArtifact(mc_exe);

    const mc_run = b.addRunArtifact(mc_exe);
    if (b.args) |args| {
        mc_run.addArgs(args);
    }
    const run_step = b.step("run", "Run mc");
    run_step.dependOn(&mc_run.step);

    @import("minsk_tests/build.zig").build(b, target, optimize, minsk_mod, minsk_runtime_mod);
}
