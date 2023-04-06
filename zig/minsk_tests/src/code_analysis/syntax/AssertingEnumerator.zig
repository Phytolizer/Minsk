const std = @import("std");
const t = @import("framework");
const SyntaxKind = @import("minsk").code_analysis.syntax.SyntaxKind;
const SyntaxNode = @import("minsk").code_analysis.syntax.SyntaxNode;
const SyntaxToken = @import("minsk").code_analysis.syntax.SyntaxToken;

allocator: std.mem.Allocator,
nodes: []const *const SyntaxNode,
i: usize = 0,

const Self = @This();

pub fn init(allocator: std.mem.Allocator, root: *const SyntaxNode) !Self {
    const nodes = try flatten(allocator, root);
    return .{
        .allocator = allocator,
        .nodes = nodes,
    };
}

pub fn deinit(self: Self) void {
    self.allocator.free(self.nodes);
}

pub fn assertEnd(self: Self, state: *t.TestState, out_msg: *[]const u8) !void {
    try t.assert(
        state,
        self.i == self.nodes.len,
        "AssertingEnumerator disposed before reaching end",
        .{},
        out_msg,
    );
}

fn flatten(allocator: std.mem.Allocator, node: *const SyntaxNode) ![]*const SyntaxNode {
    var stack = std.ArrayList(*const SyntaxNode).init(allocator);
    defer stack.deinit();
    try stack.append(node);
    var result = std.ArrayList(*const SyntaxNode).init(allocator);
    defer result.deinit();

    while (stack.popOrNull()) |n| {
        try result.append(n);

        const children = try n.children(allocator);
        defer allocator.free(children);
        std.mem.reverse(*const SyntaxNode, children);
        try stack.appendSlice(children);
    }

    return try result.toOwnedSlice();
}

pub fn assertToken(
    self: *Self,
    state: *t.TestState,
    kind: SyntaxKind,
    text: []const u8,
    out_msg: *[]const u8,
) t.TestExit!void {
    if (!std.mem.endsWith(u8, kind.displayName(), "Token")) {
        std.debug.panic("{s} is not a token kind", .{kind.displayName()});
    }

    try t.assert(state, self.i < self.nodes.len, "assertToken() at end of list", .{}, out_msg);
    const current = self.nodes[self.i];
    self.i += 1;
    try t.assert(
        state,
        current.kind == kind,
        "assertToken: {s} != {s}",
        .{ current.kind.displayName(), kind.displayName() },
        out_msg,
    );
    const token = SyntaxNode.downcast(current, SyntaxToken);
    try t.assert(
        state,
        std.mem.eql(u8, token.text, text),
        "assertToken: '{'}' != '{'}'",
        .{ std.zig.fmtEscapes(token.text), std.zig.fmtEscapes(text) },
        out_msg,
    );
}

pub fn assertNode(
    self: *Self,
    state: *t.TestState,
    kind: SyntaxKind,
    out_msg: *[]const u8,
) t.TestExit!void {
    if (std.mem.endsWith(u8, kind.displayName(), "Token")) {
        std.debug.panic("{s} is a token kind", .{kind.displayName()});
    }

    try t.assert(state, self.i < self.nodes.len, "assertNode() at end of list", .{}, out_msg);
    const current = self.nodes[self.i];
    self.i += 1;
    try t.assert(
        state,
        current.kind == kind,
        "assertNode: {s} != {s}",
        .{ current.kind.displayName(), kind.displayName() },
        out_msg,
    );
}
