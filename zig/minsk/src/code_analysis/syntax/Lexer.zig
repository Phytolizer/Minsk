const std = @import("std");
const SyntaxToken = @import("SyntaxToken.zig");
const SyntaxKind = @import("syntax_kind.zig").SyntaxKind;
const ArrayDeque = @import("ds_ext").ArrayDeque;
const glyph = @import("ziglyph");
const Object = @import("minsk_runtime").Object;
const syntax_facts = @import("syntax_facts.zig");

const AllocError = std.mem.Allocator.Error;

allocator: std.mem.Allocator,
source: []const u8,
position: usize = 0,
was_eof: bool = false,
it: std.unicode.Utf8Iterator,
peek_deq: ArrayDeque(u21),
diagnostics: std.ArrayList([]const u8),

const Self = @This();

pub fn init(allocator: std.mem.Allocator, text: []const u8) !Self {
    return .{
        .allocator = allocator,
        .source = text,
        .it = (try std.unicode.Utf8View.init(text)).iterator(),
        .peek_deq = ArrayDeque(u21).init(allocator),
        .diagnostics = std.ArrayList([]const u8).init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.peek_deq.deinit();
    for (self.diagnostics.items) |d| {
        self.allocator.free(d);
    }
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

pub fn lex(self: *Self) AllocError!?SyntaxToken {
    const start = self.position;
    var kind: SyntaxKind = .bad_token;
    var text: ?[]const u8 = null;
    var value: ?Object = null;

    if (self.was_eof) {
        return null;
    } else if (glyph.isAsciiDigit(try self.current())) {
        while (glyph.isAsciiDigit(try self.current())) {
            self.next();
        }
        text = self.source[start..self.position];
        const raw_val = std.fmt.parseInt(u63, text.?, 10) catch blk: {
            try self.diagnostics.append(try std.fmt.allocPrint(
                self.allocator,
                "ERROR: '{s}' cannot be represented as a u63",
                .{text.?},
            ));
            break :blk 0;
        };
        value = .{ .integer = raw_val };
        kind = .number_token;
    } else if (glyph.isWhiteSpace(try self.current())) {
        while (glyph.isWhiteSpace(try self.current())) {
            self.next();
        }
        kind = .whitespace_token;
    } else if (glyph.isLetter(try self.current())) {
        while (glyph.isAlphaNum(try self.current())) {
            self.next();
        }
        text = self.source[start..self.position];
        kind = syntax_facts.keywordKind(text.?);
    } else switch (try self.current()) {
        0 => {
            self.was_eof = true;
            kind = .end_of_file_token;
        },
        '+' => {
            self.next();
            kind = .plus_token;
        },
        '-' => {
            self.next();
            kind = .minus_token;
        },
        '*' => {
            self.next();
            kind = .star_token;
        },
        '/' => {
            self.next();
            kind = .slash_token;
        },
        '(' => {
            self.next();
            kind = .open_parenthesis_token;
        },
        ')' => {
            self.next();
            kind = .close_parenthesis_token;
        },
        '!' => {
            self.next();
            kind = .bang_token;
        },
        '&' => if (try self.look(1) == '&') {
            self.next();
            self.next();
            kind = .ampersand_ampersand_token;
        },
        '|' => if (try self.look(1) == '|') {
            self.next();
            self.next();
            kind = .pipe_pipe_token;
        },
        else => {},
    }
    if (kind == .bad_token) {
        var unichar_buf: [3]u8 = undefined;
        const len = std.unicode.utf8Encode(try self.current(), &unichar_buf) catch unreachable;
        try self.diagnostics.append(try std.fmt.allocPrint(
            self.allocator,
            "ERROR: bad character input: '{s}'",
            .{unichar_buf[0..len]},
        ));
        self.next();
    }
    return SyntaxToken.init(
        kind,
        start,
        text orelse self.source[start..self.position],
        value,
    );
}
