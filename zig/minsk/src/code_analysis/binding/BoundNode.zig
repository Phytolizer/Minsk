const std = @import("std");
const BoundNodeKind = @import("bound_node_kind.zig").BoundNodeKind;
const DowncastedPointer = @import("minsk_meta").DowncastedPointer;
const tty_ext = @import("tty_ext");

const AllocError = std.mem.Allocator.Error;

pub const Property = struct {
    name: []const u8,
    value: []const u8,

    fn deinit(self: Property, allocator: std.mem.Allocator) void {
        allocator.free(self.value);
    }
};

pub const DeinitFn = *const fn (self: *const Self, allocator: std.mem.Allocator) void;
pub const ChildrenFn = *const fn (self: *const Self, allocator: std.mem.Allocator) AllocError![]*const Self;
pub const PropertiesFn = ?*const fn (self: *const Self, allocator: std.mem.Allocator) AllocError![]Property;

kind: BoundNodeKind,
deinit_fn: DeinitFn,
children_fn: ChildrenFn,
properties_fn: PropertiesFn,

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

pub fn properties(self: *const Self, allocator: std.mem.Allocator) ![]Property {
    if (self.properties_fn) |properties_fn| {
        return try properties_fn(self, allocator);
    }
    return try allocator.dupe(Property, &.{});
}

fn nodeColor(kind: BoundNodeKind) tty_ext.Color {
    return if (kind.is_expression())
        .blue
    else if (kind.is_statement())
        .cyan
    else
        .yellow;
}

pub fn prettyPrint(
    self: *const Self,
    allocator: std.mem.Allocator,
    indent: []const u8,
    is_last: bool,
    writer: anytype,
    tty: ?std.io.tty.Config,
) !void {
    try tty_ext.maybeSetColor(tty, writer, .gray);
    try writer.print("{s}{s}", .{
        indent,
        if (is_last)
            "└───"
        else
            "├───",
    });
    try tty_ext.maybeSetColor(tty, writer, nodeColor(self.kind));
    try writer.writeAll(self.kind.displayName());

    const props = try self.properties(allocator);
    defer {
        for (props) |p| {
            p.deinit(allocator);
        }
        allocator.free(props);
    }
    for (props, 0..) |prop, i| {
        if (i > 0) {
            try tty_ext.maybeSetColor(tty, writer, .gray);
            try writer.writeByte(',');
        }

        try writer.writeByte(' ');
        try tty_ext.maybeSetColor(tty, writer, .yellow);
        try writer.writeAll(prop.name);
        try tty_ext.maybeSetColor(tty, writer, .gray);
        try writer.writeAll(" = ");
        try tty_ext.maybeSetColor(tty, writer, .dim_yellow);
        try writer.writeAll(prop.value);
    }

    try tty_ext.maybeSetColor(tty, writer, .reset);
    try writer.writeByte('\n');

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
            tty,
        );
    }
}
