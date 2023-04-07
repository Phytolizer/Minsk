start: usize,
length: usize,

const Self = @This();

pub fn fromBounds(start: usize, e: usize) Self {
    return .{
        .start = start,
        .length = e - start,
    };
}

pub fn end(self: Self) usize {
    return self.start + self.length;
}
