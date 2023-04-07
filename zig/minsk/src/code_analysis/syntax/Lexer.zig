const std = @import("std");
const SyntaxToken = @import("SyntaxToken.zig");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const ArrayDeque = @import("ds_ext").ArrayDeque;
const glyph = @import("ziglyph");
const Object = @import("minsk_runtime").Object;
const syntax_facts = @import("syntax_facts.zig");
const DiagnosticBag = @import("../DiagnosticBag.zig");

const AllocError = std.mem.Allocator.Error;

allocator: std.mem.Allocator,
source: []const u8,
position: usize = 0,
start: usize = 0,
kind: SyntaxKind = .bad_token,
text: ?[]const u8 = null,
value: ?Object = null,
was_eof: bool = false,
it: std.unicode.Utf8Iterator,
peek_deq: ArrayDeque(u21),
diagnostics: DiagnosticBag,

const Self = @This();

pub fn init(allocator: std.mem.Allocator, text: []const u8) !Self {
    return .{
        .allocator = allocator,
        .source = text,
        .it = (try std.unicode.Utf8View.init(text)).iterator(),
        .peek_deq = ArrayDeque(u21).init(allocator),
        .diagnostics = DiagnosticBag.init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.peek_deq.deinit();
    self.diagnostics.deinit();
}

fn look(self: *Self, n: usize) AllocError!u21 {
    try self.peek_deq.ensureTotalCapacity(n);
    var last_inserted: ?u21 = null;
    while (self.peek_deq.len < n + 1) {
        const cp = self.it.nextCodepoint() orelse return 0;
        try self.peek_deq.push(cp);
        last_inserted = cp;
    }
    return last_inserted orelse self.peek_deq.peek(n).?;
}

fn current(self: *Self) AllocError!u21 {
    return self.look(0);
}

fn next(self: *Self) void {
    const val = if (self.peek_deq.len > 0)
        self.peek_deq.pop()
    else
        self.it.nextCodepoint();
    if (val) |cp| {
        self.position += std.unicode.utf8CodepointSequenceLength(cp) catch unreachable;
    }
}

fn readNumber(self: *Self) AllocError!void {
    while (glyph.isAsciiDigit(try self.current())) {
        self.next();
    }
    self.text = self.source[self.start..self.position];
    const raw_val = std.fmt.parseInt(u63, self.text.?, 10) catch blk: {
        try self.diagnostics.reportInvalidNumber(.{
            .start = self.position,
            .length = self.text.?.len,
        }, self.text.?, u63);
        break :blk 0;
    };
    self.value = .{ .integer = raw_val };
    self.kind = .number_token;
}

fn readWhitespace(self: *Self) AllocError!void {
    while (glyph.isWhiteSpace(try self.current())) {
        self.next();
    }
    self.kind = .whitespace_token;
}

fn readIdentifierOrKeyword(self: *Self) AllocError!void {
    while (glyph.derived_core_properties.isXidContinue(try self.current())) {
        self.next();
    }
    self.text = self.source[self.start..self.position];
    self.kind = syntax_facts.keywordKind(self.text.?);
}

pub fn lex(self: *Self) AllocError!?SyntaxToken {
    self.start = self.position;
    self.kind = .bad_token;
    self.text = null;
    self.value = null;

    if (self.was_eof) {
        return null;
    }
    switch (try self.current()) {
        0 => {
            self.was_eof = true;
            self.kind = .end_of_file_token;
        },
        '+' => {
            self.next();
            self.kind = .plus_token;
        },
        '-' => {
            self.next();
            self.kind = .minus_token;
        },
        '*' => {
            self.next();
            self.kind = .star_token;
        },
        '/' => {
            self.next();
            self.kind = .slash_token;
        },
        '(' => {
            self.next();
            self.kind = .open_parenthesis_token;
        },
        ')' => {
            self.next();
            self.kind = .close_parenthesis_token;
        },
        '!' => if (try self.look(1) == '=') {
            self.next();
            self.next();
            self.kind = .bang_equals_token;
        } else {
            self.next();
            self.kind = .bang_token;
        },
        '&' => if (try self.look(1) == '&') {
            self.next();
            self.next();
            self.kind = .ampersand_ampersand_token;
        },
        '|' => if (try self.look(1) == '|') {
            self.next();
            self.next();
            self.kind = .pipe_pipe_token;
        },
        '=' => if (try self.look(1) == '=') {
            self.next();
            self.next();
            self.kind = .equals_equals_token;
        } else {
            self.next();
            self.kind = .equals_token;
        },
        '0'...'9' => try self.readNumber(),
        ' ', '\r', '\t', '\n' => try self.readWhitespace(),
        'a'...'z', 'A'...'Z' => try self.readIdentifierOrKeyword(),
        else => if (glyph.isAsciiDigit(try self.current())) {
            try self.readNumber();
        } else if (glyph.isWhiteSpace(try self.current())) {
            try self.readWhitespace();
        } else if (glyph.derived_core_properties.isXidStart(try self.current())) {
            try self.readIdentifierOrKeyword();
        },
    }
    if (self.kind == .bad_token) {
        try self.diagnostics.reportBadCharacter(self.position, try self.current());
        self.next();
    }
    return SyntaxToken.init(
        self.kind,
        self.start,
        self.text orelse
            syntax_facts.getText(self.kind) orelse
            self.source[self.start..self.position],
        self.value,
    );
}
