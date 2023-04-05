pub const syntax = @import("code_analysis/syntax.zig");
pub const Compilation = @import("code_analysis/Compilation.zig");
pub const VariableSymbol = @import("code_analysis/VariableSymbol.zig");

comptime {
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
}
