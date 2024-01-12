const std = @import("std");

pub fn build(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.Mode,
    minsk: *std.Build.Module,
    minsk_runtime: *std.Build.Module,
) void {
    const this_dir = comptime std.fs.path.dirname(@src().file) orelse ".";
    const exe = b.addExecutable(.{
        .name = "minsk_tests",
        .root_source_file = .{ .path = this_dir ++ "/src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("minsk", minsk);
    exe.root_module.addImport("minsk_runtime", minsk_runtime);
    exe.root_module.addAnonymousImport("framework", .{
        .root_source_file = .{ .path = this_dir ++ "/framework.zig" },
    });
    _ = b.addInstallArtifact(exe, .{});

    const run = b.addRunArtifact(exe);
    if (b.args) |args| {
        run.addArgs(args);
    }
    const run_step = b.step("test", "Test minsk");
    run_step.dependOn(&run.step);
}
