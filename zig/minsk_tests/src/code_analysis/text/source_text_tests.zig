const std = @import("std");
const t = @import("framework");
const SourceText = @import("minsk").code_analysis.text.SourceText;

const IncludesLastLineTest = struct {
    text: []const u8,
    line_count: usize,
    fn init(text: []const u8, line_count: usize) @This() {
        return .{ .text = text, .line_count = line_count };
    }
};
fn includesLastLine(state: *t.TestState, ctx: IncludesLastLineTest, out_msg: *[]const u8) t.TestExit!void {
    const source_text = SourceText.from(t.allocator, ctx.text) catch unreachable;
    defer source_text.deinit();

    try t.assert(
        state,
        source_text.lines.len == ctx.line_count,
        "line count mismatch: {d} != {d}",
        .{ source_text.lines.len, ctx.line_count },
        out_msg,
    );
}

pub fn sourceTextSuite(state: *t.TestState) void {
    const ill = IncludesLastLineTest.init;
    for ([_]IncludesLastLineTest{
        ill(".", 1),
        ill(".\r\n", 2),
        ill(".\r\n\r\n", 3),
    }) |tt| {
        t.runTest(
            state,
            IncludesLastLineTest,
            includesLastLine,
            tt,
            std.fmt.allocPrint(
                t.allocator,
                "includes last line ('{'}', {d})",
                .{ std.zig.fmtEscapes(tt.text), tt.line_count },
            ) catch unreachable,
            .allocated,
        );
    }
}
