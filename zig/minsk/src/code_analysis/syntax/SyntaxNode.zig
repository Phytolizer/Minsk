const std = @import("std");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const SyntaxToken = @import("SyntaxToken.zig");
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;
const TextSpan = @import("../text/TextSpan.zig");
const tty_ext = @import("tty_ext");

const AllocError = std.mem.Allocator.Error;

pub const DeinitFn = *const fn (self: *const Self, allocator: std.mem.Allocator) void;
pub const ChildrenFn = *const fn (self: *const Self, allocator: std.mem.Allocator) AllocError![]*const Self;
pub const SpanFn = *const fn (self: *const Self, allocator: std.mem.Allocator) AllocError!TextSpan;

kind: SyntaxKind,
deinit_fn: DeinitFn,
children_fn: ChildrenFn,
span_fn: ?SpanFn,

const Self = @This();

pub fn downcast(self: anytype, comptime T: type) DowncastedPointer(@TypeOf(self), T) {
    return @fieldParentPtr(T, "base", self);
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    self.deinit_fn(self, allocator);
}

pub fn children(self: *const Self, allocator: std.mem.Allocator) ![]*const Self {
    return try self.children_fn(self, allocator);
}

pub fn span(self: *const Self, allocator: std.mem.Allocator) !TextSpan {
    if (self.span_fn) |span_fn| {
        return span_fn(self, allocator);
    }
    const cs = try self.children(allocator);
    defer allocator.free(cs);
    const first = try cs[0].span(allocator);
    const last = try cs[cs.len - 1].span(allocator);
    return TextSpan.fromBounds(first.start, last.end());
}

pub fn prettyPrint(
    self: *const Self,
    allocator: std.mem.Allocator,
    indent: []const u8,
    is_last: bool,
    writer: anytype,
    comptime colors: enum { colors, no_colors },
    tty: ?std.debug.TTY.Config,
    buf_writer: anytype,
) !void {
    if (colors == .colors) {
        try tty_ext.setColor(tty.?, writer, .gray);
    }
    try writer.print("{s}{s}", .{
        indent,
        if (is_last)
            "└───"
        else
            "├───",
    });
    const is_token = std.mem.endsWith(u8, self.kind.displayName(), "Token");
    if (colors == .colors) {
        try tty_ext.setColor(tty.?, writer, if (is_token)
            .cyan
        else
            .blue);
    }
    try writer.writeAll(self.kind.displayName());
    if (is_token) {
        const token = self.downcast(SyntaxToken);
        if (token.value) |value| {
            if (colors == .colors) {
                try tty_ext.setColor(tty.?, writer, .magenta);
            }
            try writer.print(" {}", .{value});
        }
    }
    if (colors == .colors) {
        tty_ext.resetColor(tty.?, buf_writer);
    }
    try writer.writeAll("\n");
    const new_indent = try std.mem.concat(allocator, u8, &.{
        indent, if (is_last)
            "    "
        else
            "│   ",
    });
    defer allocator.free(new_indent);

    const cs = try self.children(allocator);
    defer allocator.free(cs);
    for (cs, 0..) |c, i| {
        try c.prettyPrint(
            allocator,
            new_indent,
            i == cs.len - 1,
            writer,
            colors,
            tty,
            buf_writer,
        );
    }
}
