const std = @import("std");
const Diagnostic = @import("Diagnostic.zig");
const TextSpan = @import("text/TextSpan.zig");
const SyntaxKind = @import("syntax/syntax_kind.zig").SyntaxKind;
const Object = @import("minsk_runtime").Object;

allocator: std.mem.Allocator,
diagnostics: std.ArrayList(Diagnostic),

const Self = @This();

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .diagnostics = std.ArrayList(Diagnostic).init(allocator),
    };
}

pub fn clear(self: *Self) void {
    self.diagnostics.clearAndFree();
}

pub fn deinit(self: Self) void {
    for (self.diagnostics.items) |d| {
        d.deinit(self.allocator);
    }
    self.diagnostics.deinit();
}

pub fn extend(self: *Self, other: *Self) !void {
    try self.diagnostics.appendSlice(other.diagnostics.items);
    other.diagnostics.clearAndFree();
}

fn report(self: *Self, span: TextSpan, message: []const u8) !void {
    try self.diagnostics.append(Diagnostic{ .span = span, .message = message });
}

pub fn reportInvalidNumber(self: *Self, span: TextSpan, text: []const u8, comptime T: type) !void {
    const message = try std.fmt.allocPrint(
        self.allocator,
        "The number {s} isn't a valid {s}.",
        .{ text, @typeName(T) },
    );
    try self.report(span, message);
}

pub fn reportBadCharacter(self: *Self, position: usize, cp: u21) !void {
    var unichar_buf: [8]u8 = undefined;
    const len = std.unicode.utf8Encode(cp, &unichar_buf) catch unreachable;
    const message = try std.fmt.allocPrint(
        self.allocator,
        "Bad character in input: '{s}'",
        .{unichar_buf[0..len]},
    );
    try self.report(.{ .start = position, .length = 1 }, message);
}

pub fn reportUnexpectedToken(
    self: *Self,
    span: TextSpan,
    actual_kind: SyntaxKind,
    expected_kind: SyntaxKind,
) !void {
    const message = try std.fmt.allocPrint(
        self.allocator,
        "Unexpected token <{s}>, expected <{s}>.",
        .{ actual_kind.displayName(), expected_kind.displayName() },
    );
    try self.report(span, message);
}

pub fn reportUndefinedBinaryOperator(
    self: *Self,
    span: TextSpan,
    operator_text: []const u8,
    left_type: Object.Type,
    right_type: Object.Type,
) !void {
    const message = try std.fmt.allocPrint(
        self.allocator,
        "Binary operator '{s}' is not defined for types {s} and {s}.",
        .{
            operator_text,
            left_type.displayName(),
            right_type.displayName(),
        },
    );
    try self.report(span, message);
}

pub fn reportUndefinedUnaryOperator(
    self: *Self,
    span: TextSpan,
    operator_text: []const u8,
    operand_type: Object.Type,
) !void {
    const message = try std.fmt.allocPrint(
        self.allocator,
        "Unary operator '{s}' is not defined for type {s}.",
        .{
            operator_text,
            operand_type.displayName(),
        },
    );
    try self.report(span, message);
}

pub fn reportUndefinedName(self: *Self, span: TextSpan, name: []const u8) !void {
    const message = try std.fmt.allocPrint(
        self.allocator,
        "Variable '{s}' doesn't exist.",
        .{name},
    );
    try self.report(span, message);
}

pub fn reportVariableAlreadyDeclared(self: *Self, span: TextSpan, name: []const u8) !void {
    const message = try std.fmt.allocPrint(
        self.allocator,
        "Variable '{s}' has already been declared.",
        .{name},
    );
    try self.report(span, message);
}

pub fn reportCannotConvert(self: *Self, span: TextSpan, from_type: Object.Type, to_type: Object.Type) !void {
    const message = try std.fmt.allocPrint(
        self.allocator,
        "Cannot convert type {s} to type {s}.",
        .{ from_type.displayName(), to_type.displayName() },
    );
    try self.report(span, message);
}

pub fn reportCannotAssign(self: *Self, span: TextSpan, name: []const u8) !void {
    const message = try std.fmt.allocPrint(
        self.allocator,
        "Variable '{s}' is read-only and cannot be assigned to.",
        .{name},
    );
    try self.report(span, message);
}
