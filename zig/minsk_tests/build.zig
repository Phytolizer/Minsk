const std = @import("std");

pub fn build(
    b: *std.Build,
    target: std.zig.CrossTarget,
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
    exe.addModule("minsk", minsk);
    exe.addModule("minsk_runtime", minsk_runtime);
    exe.addAnonymousModule("framework", .{
        .source_file = .{ .path = this_dir ++ "/framework.zig" },
    });
    exe.install();

    const run = b.addRunArtifact(exe);
    if (b.args) |args| {
        run.addArgs(args);
    }
    const run_step = b.step("test", "Test minsk");
    run_step.dependOn(&run.step);
}
