const std = @import("std");
const VariableSymbol = @import("../VariableSymbol.zig");

allocator: std.mem.Allocator,
parent: ?*BoundScope,
variables: Map,

const BoundScope = @This();
const Map = std.StringArrayHashMap(VariableSymbol);

pub fn init(allocator: std.mem.Allocator, parent: ?*BoundScope) !*BoundScope {
    const result = try allocator.create(BoundScope);
    result.* = .{
        .parent = parent,
        .allocator = allocator,
        .variables = Map.init(allocator),
    };
    return result;
}

pub fn deinit(
    self: *BoundScope,
    comptime with_parents: enum { with_parents, without_parents },
) void {
    if (with_parents == .with_parents) if (self.parent) |p| p.deinit(.with_parents);

    for (self.variables.keys()) |k| {
        self.allocator.free(k);
    }
    self.variables.deinit();
    self.allocator.destroy(self);
}

pub fn tryDeclare(self: *BoundScope, variable: VariableSymbol) !bool {
    const result = try self.variables.getOrPut(variable.name);
    if (result.found_existing) return false;

    result.key_ptr.* = try self.allocator.dupe(u8, variable.name);
    result.value_ptr.* = variable;
    return true;
}
pub fn tryLookup(self: *const BoundScope, name: []const u8) ?VariableSymbol {
    if (self.variables.get(name)) |v| return v;
    if (self.parent) |p| return p.tryLookup(name);
    return null;
}

pub fn getDeclaredVariables(self: BoundScope) ![]const VariableSymbol {
    const result = try self.allocator.alloc(VariableSymbol, self.variables.values().len);
    errdefer self.allocator.free(result);
    for (self.variables.values(), result) |v, *r| {
        r.* = VariableSymbol{
            .duped = true,
            .name = try self.allocator.dupe(u8, v.name),
            .ty = v.ty,
            .is_read_only = v.is_read_only,
        };
    }
    return result;
}
