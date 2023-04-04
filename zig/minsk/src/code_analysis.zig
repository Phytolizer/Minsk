pub const syntax = @import("code_analysis/syntax.zig");
pub const Compilation = @import("code_analysis/Compilation.zig");

comptime {
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}
