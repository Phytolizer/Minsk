const std = @import("std");

pub fn ArrayDeque(comptime T: type) type {
    return struct {
        buf: []T,
        begin: usize,
        end: usize,
        full: bool,
        len: usize,
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .buf = &.{},
                .begin = 0,
                .end = 0,
                .full = true,
                .len = 0,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: Self) void {
            self.allocator.free(self.buf);
        }

        fn grow(self: *Self, len: usize) !void {
            const new_buf = try self.allocator.alloc(T, len);
            const is_divided = self.begin > self.end;
            if (is_divided) {
                @memcpy(new_buf[0..(self.buf.len - self.begin)], self.buf[self.begin..]);
                const offset = self.buf.len - self.begin;
                @memcpy(new_buf[offset..(offset + self.end)], self.buf[0..self.end]);
            } else {
                @memcpy(new_buf[0..(self.end - self.begin)], self.buf[self.begin..self.end]);
            }
            self.begin = 0;
            self.end = self.len;
            self.allocator.free(self.buf);
            self.buf = new_buf;
        }

        pub fn ensureTotalCapacity(self: *Self, target: usize) !void {
            var cap = self.buf.len;
            while (cap < target) {
                const min_size = 8;
                cap = @max(
                    cap + @divTrunc(cap, 2),
                    min_size,
                );
            }
            try self.grow(cap);
        }

        pub fn push(self: *Self, value: T) !void {
            if (self.full) {
                try self.ensureTotalCapacity(self.len + 1);
            }

            self.buf[self.end] = value;
            self.end = @mod(self.end + 1, self.buf.len);
            if (self.end == self.begin) {
                self.full = true;
            }
            self.len += 1;
        }

        pub fn pop(self: *Self) ?T {
            if (!self.full and self.end == self.begin) {
                return null;
            }
            const result = self.buf[self.begin];
            self.begin = @mod(self.begin + 1, self.buf.len);
            self.full = false;
            self.len -= 1;
            return result;
        }

        pub fn peek(self: *Self, n: usize) ?T {
            if (n > self.len)
                return null;

            const idx = @mod(self.begin + n, self.buf.len);
            return self.buf[idx];
        }
    };
}
