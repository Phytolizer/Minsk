pub fn DowncastedPointer(comptime T: type, comptime U: type) type {
    return switch (@typeInfo(T)) {
        .pointer => |p| if (p.is_const)
            *const U
        else
            *U,
        else => @compileError("not a pointer"),
    };
}
