const std = @import("std");
const Object = @import("minsk_runtime").Object;

name: []const u8,
ty: Object.Type,
is_read_only: bool,
duped: bool = false,

const VariableSymbol = @This();

pub fn deinit(self: VariableSymbol, allocator: std.mem.Allocator) void {
    if (self.duped) allocator.free(self.name);
}

pub const Map = std.ArrayHashMap(
    VariableSymbol,
    Object,
    struct {
        pub fn hash(ctx: @This(), key: VariableSymbol) u32 {
            _ = ctx;
            var hasher = std.hash.Wyhash.init(0);
            hasher.update(key.name);
            hasher.update(std.mem.asBytes(&key.ty));
            const result = @truncate(u32, hasher.final());
            return result;
        }
        pub fn eql(ctx: @This(), a: VariableSymbol, b: VariableSymbol, b_index: usize) bool {
            _ = ctx;
            _ = b_index;
            return a.ty == b.ty and std.mem.eql(u8, a.name, b.name);
        }
    },
    true,
);

pub const MapExt = struct {
    pub fn matchName(map: *Map, name: []const u8) ?VariableSymbol {
        for (map.keys()) |k| {
            if (std.mem.eql(u8, k.name, name)) {
                return k;
            }
        }
        return null;
    }
};
