const std = @import("std");
const BoundTreeRewriter = @import("../binding/BoundTreeRewriter.zig");

base: BoundTreeRewriter,

const Self = @This();

pub fn init(allocator: std.mem.Allocator) !*BoundTreeRewriter {
    const self = try allocator.create(Self);
    self.base = .{
        .allocator = allocator,
        .rewrite_fns = .{},
        .deinit_fn = deinit,
    };
    return &self.base;
}

fn deinit(base: *const BoundTreeRewriter) void {
    const self = base.downcast(Self);
    self.base.allocator.destroy(self);
}
