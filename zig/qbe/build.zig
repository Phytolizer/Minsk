const std = @import("std");
const builtin = @import("builtin");

pub fn build(
    b: *std.Build,
    target: std.zig.CrossTarget,
    optimize: std.builtin.Mode,
) void {
    const tgtstring = switch (target.os_tag orelse builtin.os.tag) {
        .macos => switch (target.cpu_arch orelse builtin.cpu.arch) {
            .aarch64 => "T_arm64_apple",
            else => "T_amd64_apple",
        },
        else => switch (target.cpu_arch orelse builtin.cpu.arch) {
            .aarch64 => "T_arm64",
            .riscv64 => "T_rv64",
            else => "T_amd64_sysv",
        },
    };

    const this_dir = comptime std.fs.path.dirname(@src().file) orelse ".";
    const config_h = b.addConfigHeader(.{}, .{});
    config_h.values.put("Deftgt", .{ .ident = tgtstring }) catch unreachable;
    const cflags = [_][]const u8{
        "-Wall",
        "-Wextra",
        "-std=c99",
        "-g",
        "-Wpedantic",
    };

    const common_obj = [_][]const u8{
        "main", "util",  "parse", "abi",   "cfg",
        "mem",  "ssa",   "alias", "load",  "copy",
        "fold", "simpl", "live",  "spill", "rega",
        "emit",
    };
    const amd64_obj = [_][]const u8{
        "amd64/targ", "amd64/sysv", "amd64/isel", "amd64/emit",
    };
    const arm64_obj = [_][]const u8{
        "arm64/targ", "arm64/abi", "arm64/isel", "arm64/emit",
    };
    const rv64_obj = [_][]const u8{
        "rv64/targ", "rv64/abi", "rv64/isel", "rv64/emit",
    };
    const obj = &common_obj ++ &amd64_obj ++ &arm64_obj ++ &rv64_obj;
    const src = comptime blk: {
        var result: []const []const u8 = &.{};
        for (obj) |o| {
            result = result ++
                &[_][]const u8{this_dir ++ "/src/" ++ o ++ ".c"};
        }
        break :blk result;
    };

    const qbe_exe = b.addExecutable(.{
        .name = "qbe",
        .target = target,
        .optimize = optimize,
    });
    qbe_exe.linkLibC();
    qbe_exe.addCSourceFiles(src, &cflags);
    qbe_exe.addIncludePath(this_dir ++ "/src");
    qbe_exe.step.dependOn(&config_h.step);
    qbe_exe.install();
    qbe_exe.addConfigHeader(config_h);
}
