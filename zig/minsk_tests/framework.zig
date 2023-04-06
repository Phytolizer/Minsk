const std = @import("std");

/// Test setup will change this to an actual working allocator
pub var allocator: std.mem.Allocator = std.testing.failing_allocator;

pub const TestState = struct {
    passed: usize = 0,
    failed: usize = 0,
    skipped: usize = 0,
    assertions: usize = 0,
    verbose: bool = false,
};

pub const TestExit = error{
    TestFailed,
    TestSkipped,
};

pub fn assert(
    state: *TestState,
    condition: bool,
    comptime fmt: []const u8,
    args: anytype,
    out_msg: *[]const u8,
) TestExit!void {
    state.assertions += 1;
    if (condition) return;

    out_msg.* = std.fmt.allocPrint(allocator, fmt, args) catch unreachable;
    return error.TestFailed;
}

pub fn assertSingle(
    state: *TestState,
    comptime T: type,
    slice: []const T,
    comptime fmt: []const u8,
    args: anytype,
    out_msg: *[]const u8,
) TestExit!T {
    try assert(state, slice.len == 1, fmt, args, out_msg);
    return slice[0];
}

const Allocated = enum { allocated, static };

pub fn runSuite(
    state: *TestState,
    comptime f: SuiteFn,
    display_name: []const u8,
    allocated: Allocated,
) void {
    std.debug.print("SUITE {s}\n", .{display_name});
    if (allocated == .allocated)
        allocator.free(display_name);
    f(state);
}

pub fn runTest(
    state: *TestState,
    comptime Ctx: type,
    comptime f: TestFn(Ctx),
    ctx: Ctx,
    display_name: []const u8,
    allocated: Allocated,
) void {
    defer if (allocated == .allocated) allocator.free(display_name);
    if (state.verbose) {
        std.debug.print("TEST  {s}\n", .{display_name});
    }
    var msg: []const u8 = undefined;
    if (f(state, ctx, &msg)) |_| {
        state.passed += 1;
    } else |e| switch (e) {
        error.TestFailed => {
            std.debug.print("FAIL  {s}: {s}\n", .{ display_name, msg });
            allocator.free(msg);
            state.failed += 1;
        },
        error.TestSkipped => {
            std.debug.print("SKIP  {s}\n", .{display_name});
            state.skipped += 1;
        },
    }
}

pub fn TestFn(comptime Ctx: type) type {
    return fn (state: *TestState, ctx: Ctx, out_msg: *[]const u8) TestExit!void;
}

pub const SuiteFn = fn (state: *TestState) void;
