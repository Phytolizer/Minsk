start: usize,
length: usize,

const Self = @This();

pub fn end(self: Self) usize {
    return self.start + self.length;
}
